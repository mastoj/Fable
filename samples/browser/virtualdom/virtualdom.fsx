(**
 - title: Super Fable Mario
 - tagline: Mario clone using HTML5 canvas
 - app-style: height:384px; width:512px; margin:20px auto 20px auto; position:relative;
 - intro: Mario clone, based on functional reactive [sample written in
   Elm](http://debug.elm-lang.org/edit/Mario.elm). The Fable version is using HTML 5
   canvas to render the background and an `img` tag showing the Mario (using animated GIFs).
   You can find the [full source code on GitHub](https://github.com/fsprojects/Fable/blob/master/samples/browser/mario/mario.fsx).

   To see how it works, use the left, right and up buttons to play. Our Mario respects
   no boundaries!
*)
(*** hide ***)
#r "node_modules/fable-core/Fable.Core.dll"
#load "html.fsx"
open Fable.Core
//open Fable.Import.Browser

type private QueueCell<'T> =
  { Value : 'T
    mutable Next : option<QueueCell<'T>> }

/// Simple queue implemented as linked list with `Add`
/// to add things to the end & `TryGet` to remove first item
type private Queue<'T>() =
    let mutable firstAndLast = None

    member x.Add(it:'T) =
        let itCell = { Value = it; Next = None }
        match firstAndLast with
        | None -> firstAndLast <- Some(itCell, itCell)
        | Some(first, last) ->
            last.Next <- Some itCell
            firstAndLast <- Some(first, itCell)

    member x.TryGet() =
        match firstAndLast with
        | None -> None
        | Some({ Value = value; Next = None}, _) ->
            firstAndLast <- None
            Some value
        | Some({ Value = value; Next = Some next}, last) ->
            firstAndLast <- Some(next, last)
            Some value

/// Represents a reply channel used by the `PostAndAsyncReply` method of
/// `MailboxProcessor<'T>` (same idea as standard `AsyncReplyChannel<'T>`)
type ReplyChannel<'T>(f:'T -> unit) =
    member x.Reply(res) = f res

/// A lightweight implementation of F# `MailboxProcessor`. As the standard one,
/// you can start it using `MailboxProcessor.Start(fun inbox -> ...)` and you
/// then call `inbox.Receive` inside the body and `agent.Post` anywhere.
type MailboxProcessor<'T>(body) =
    let mutable messages = Queue<'T>()
    let mutable continuation = None
    let processEvents () =
        continuation |> Option.iter (fun cont ->
            let fm = messages.TryGet()
            fm |> Option.iter (fun value ->
              continuation <- None
              cont value ) )

    member x.Start() =
        body x |> Async.StartImmediate

    static member Start(body) =
        let mbox = MailboxProcessor<_>(body)
        mbox.Start()
        mbox

    member x.Receive() = Async.FromContinuations(fun c ->
        if Option.isSome continuation then failwith "Receive can only be called once!"
        continuation <- Some (unbox c) // Here be dragons
        processEvents () )

    member x.PostAndAsyncReply<'R>(f) : Async<'R> =
        let result = ref None
        let continuation = ref None
        let checkCompletion () =
            match result.Value, continuation.Value with
            | Some res, Some cont -> cont res
            | _ -> ()
        let reply =
            ReplyChannel<'R>(fun res ->
                result.Value <- Some res
                checkCompletion () )
        messages.Add(f reply)
        processEvents ()
        Async.FromContinuations(fun (c, _, _) ->
            continuation.Value <- Some c
            checkCompletion ())

    member x.Post(msg) =
        messages.Add(msg)
        processEvents ()

open Fable.Import.Browser
let failwithjs() = failwith "JS only"
let [<Import("default","virtual-dom/h")>] h(arg1: string, arg2: obj, arg3: obj[]): obj = failwithjs()
let [<Import("default","virtual-dom/diff")>] diff(arg1:obj, arg2:obj): obj = failwithjs()
let [<Import("default","virtual-dom/patch")>] patch(arg1:obj, arg2:obj): Node = failwithjs()
let [<Import("default","virtual-dom/create-element")>] createElement:obj -> Node = failwithjs()

[<Emit("String($0)")>]
let String i :obj= failwith "JS only"

[<Emit("x + y")>]
let something x y = failwith "JS only"

[<Emit("$1.join($0)")>]
let join sep strs = failwith "JS only"


module VDom =
    open Html

    let rec render node =
//        let renderMouseEventHandler (eventType, handler) = eventType, (handler :> obj)
        let renderEventHandler (eventType, handler) = eventType, handler

        let renderEventBinding binding =
            match binding with
            | MouseEventHandler (eventType, handler) -> (eventType, handler :> obj)//renderMouseEventHandler mh
            //| KeyboardEventHandler (eventType, handler) -> (eventType, handler :> obj)
            | EventHandler (eventType, handler) -> (eventType, handler :> obj)
//            | x ->
                //printfn "Missing renderer for handler: %A" x
//                raise (exn "Missing renderer for handler")
            |> renderEventHandler

        let toAttrs attrs =
            attrs
            |> List.map (function
                    | EventHandlerBinding binding -> binding |> renderEventBinding
                    | Style style -> "style", ((style |> Array.map (fun (k,v) -> k + ":" + v) |> join ";") :> obj)
                    | KeyValue (key, value) -> key,(value :> obj)
                )
            |> createObj

        match node with
        | Element((tag,attrs), nodes) ->
            let hAttrs = attrs |> toAttrs
            let children = nodes |> List.map render |> List.toArray
            h(tag, hAttrs, children)

        | VoidElement (tag, attrs) ->
            let hAttrs = attrs |> toAttrs
            h(tag, hAttrs, [||])
        | Text str -> String str
        | WhiteSpace str -> String str

type AppState<'TModel, 'TMessage> = {
        Model: 'TModel;
        View: 'TModel -> ('TMessage -> unit) -> Html.Node;
        Update: 'TMessage -> 'TModel -> 'TModel}

type Observer<'T>(next, error, completed) =
    interface System.IObserver<'T> with
        member x.OnCompleted() = completed()
        member x.OnError(e) = error e
        member x.OnNext(v) = next v

type App<'TModel, 'TMessage> = {AppState: AppState<'TModel, 'TMessage>; Node: Node option; CurrentTree: obj option}

let start app =
    let createTree view model handler =
        view model handler
        |> VDom.render

    MailboxProcessor.Start(fun inbox ->
        let rec loop state =
            async {
                match state.Node, state.CurrentTree with
                | None,_ ->
                    let tree = createTree state.AppState.View state.AppState.Model inbox.Post
                    let rootNode = createElement tree
                    document.body.appendChild(rootNode) |> ignore
                    return! loop {state with CurrentTree = Some tree; Node = Some rootNode}
                | Some rootNode, Some currentTree ->
                    let! message = inbox.Receive()
                    try
                        let model' = state.AppState.Update message state.AppState.Model
                        let tree = createTree state.AppState.View model' inbox.Post
                        let patches = diff(currentTree, tree)
                        patch(rootNode, patches) |> ignore
                        return! loop {state with AppState = {state.AppState with Model = model'}; CurrentTree = Some tree}
                    with
                        x ->
                            printfn "Error updating model: %A" x
                            return! loop state
                | _ -> failwith "Shouldn't happen"
            }
        loop {AppState = app; Node = None; CurrentTree = None})

open VDom
open Html

type Item =
    {
        Name: string
        Done: bool
        Id: int
    }
type Model =
    {
        Counter: int
        Items: Item list
        Input: string
    }
type Action =
    | Increment
    | IncrementWith of int
    | Decrement
    | DecrementWith of int
    | AddItem of Item
    | ChangeInput of string
    | MarkAsDone of Item

let view m handler =
    let itemList items =
        ul [attribute "className" "todo-list"]
            (items |> List.map (fun i ->
                li [    attribute "className" ("todo-item " + (if i.Done then "done" else ""))
                        onMouseClick (fun x -> handler (MarkAsDone i))] [text i.Name]))

    div
        [
            Style [|"border","1px solid red"|]
        ]
        [
            div [Style [|"border","1px solid blue"|]; onMouseClick (fun x -> handler Increment); onDblClick (fun x -> handler (IncrementWith 100))] [Html.Text (string "Increment")]
            text (string m.Counter)
            br []
            span [] [text "Hello world"]
            hr []
            (m.Items |> itemList)
            input
                [   attribute "value" m.Input
                    attribute "type" "text"
                    onKeydown (fun x ->
                        if x.code = "Enter"
                        then handler (AddItem {Name = m.Input; Id = 0; Done = false})
                        else printfn "Yolo: %s" x.code)
                    onKeyup (fun x -> printfn "Trying to change %A" (x?target?value); handler (ChangeInput (x?target?value :?> string)))]
            button [    attribute "name" "Click me"
                        onMouseClick (fun _ -> handler (AddItem {Name = m.Input; Id = 0; Done = false}))
                    ]
                    [text "Click me"]
            div [Html.Style [|"border", "1px solid green"; "height", ((string (70+m.Counter)) + "px")|]; onMouseClick (fun x -> handler Decrement); onDblClick (fun x -> handler (DecrementWith 50))] [Html.Text (string "Decrement")]
        ]

let update msg model =
    let model' =
        printfn "Received: %A" msg
        match msg with
        | Increment -> {model with Counter = model.Counter + 1}
        | IncrementWith x -> {model with Counter = model.Counter + x}
        | Decrement -> {model with Counter = model.Counter - 1}
        | DecrementWith x -> {model with Counter = model.Counter - 1}
        | AddItem item ->
            let maxId =
                if model.Items = [] then 1
                else
                    model.Items
                    |> List.map (fun x -> x.Id)
                    |> List.maxBy (fun x -> x)
            let item' = {item with Id = maxId + 1}
            {model with Items = item'::model.Items; Input = ""}
        | ChangeInput v -> {model with Input = v}
        | MarkAsDone i ->
            let items' =
                model.Items |> List.map (fun i' -> if i' <> i then i' else {i with Done = true})
            {model with Items = items'}
    printfn "New model %A" model'
    model'

start {Model = {Counter = 0; Items = []; Input = ""}; View = view; Update = update}
