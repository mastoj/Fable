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

module Html =
//    type Attribute = string * string

    type MouseEvent =
        {
            altKey: bool
            screenX: int
            screenY: int
        }

    type MouseEventHandler =
        | OnClick of (MouseEvent -> unit)

    type EventHandler =
        | MouseEventHandler of MouseEventHandler

    type Style =
        { border : string }

    type KeyValue = string*string

    type Attribute =
    | EventHandler of EventHandler
    | Style of Style
    | KeyValue of KeyValue

    type Element = string * Attribute list
    /// A Node in Html have the following forms
    type Node =
    /// A regular html element that can contain a list of other nodes
    | Element of Element * Node list
    /// A void element is one that can't have content, like link, br, hr, meta
    /// See: https://dev.w3.org/html5/html-author/#void
    | VoidElement of Element
    /// A text value for a node
    | Text of string
    /// Whitespace for formatting
    | WhiteSpace of string

    let mouseClick f =
        EventHandler (MouseEventHandler (OnClick f))


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

module VDom =
    open Html

    let rec render node =
        printfn "%A" node

        let renderMouseEventHandler = function
            | OnClick h -> "onclick", (h :> obj)

        let renderHandler = function
            | MouseEventHandler mh -> renderMouseEventHandler mh

        let toAttrs attrs =
            attrs
            |> List.map (function
                    | EventHandler handler -> handler |> renderHandler
                    | Style style -> "style", (style :> obj)
                    | KeyValue (key, value) -> key,(value :> obj)
                )
            |> createObj

        match node with
        | Element((tag,attrs), nodes) ->
            let hAttrs = attrs |> toAttrs
            let children = nodes |> List.map render |> Array.ofList
            h(tag, hAttrs, children)

        | VoidElement el -> String ""
        | Text str -> String str
        | WhiteSpace str -> String str

    let div attrs children = Element(("div", attrs), children)

type AppState<'TModel, 'TMessage> = {
        Model: 'TModel;
        View: 'TModel -> ('TMessage -> unit) -> Html.Node;
        Update: 'TMessage -> 'TModel -> 'TModel}

type Observer<'T>(next, error, completed) =
    interface System.IObserver<'T> with
        member x.OnCompleted() = completed()
        member x.OnError(e) = error e
        member x.OnNext(v) = next v

type Microsoft.FSharp.Control.Async with
  /// Returns an asynchronous workflow that waits for the first
  /// occurrence of the specified event & then unsubscribes
  static member AwaitObservable (obs:System.IObservable<'T>) =
    Async.FromContinuations(fun (cont, econt, ccont) ->
        let disp : ref<option<System.IDisposable>> = ref None
        let onNext v =
          disp.Value.Value.Dispose()
          cont(v)
        let onError e =
          disp.Value.Value.Dispose()
          econt(e)
        let onComplete () =
          disp.Value.Value.Dispose()
          ccont (unbox (System.Exception("Observable stream ended.")))
        disp := obs.Subscribe(Observer(onNext, onError, onComplete)) |> Some)


type App<'TModel, 'TMessage> = {AppState: AppState<'TModel, 'TMessage>; Node: Node option; CurrentTree: obj option}

type Action =
    | Increment
    | Decrement

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
                    let model' = state.AppState.Update message state.AppState.Model
                    let tree = createTree state.AppState.View model' inbox.Post
                    let patches = diff(currentTree, tree)
                    patch(rootNode, patches) |> ignore
                    return! loop {state with AppState = {state.AppState with Model = model'}; CurrentTree = Some tree}
                | _ -> failwith "Shouldn't happen"
            }
        loop {AppState = app; Node = None; CurrentTree = None})


//    let inbox = new Event<Action>()
//
//    let rec loop state =
//        async {
//            match state.Node, state.CurrentTree with
//            | None,_ ->
//                let tree = createTree state.AppState.View state.AppState.Model inbox.Trigger
//                let rootNode = createElement tree
//                document.body.appendChild(rootNode) |> ignore
//                return! loop {state with CurrentTree = Some tree; Node = Some rootNode}
//
//            | Some rootNode, Some currentTree ->
//                let! message = inbox.Publish |> Async.AwaitObservable
//                let model' = state.AppState.Update message state.AppState.Model
//                let tree = createTree state.AppState.View model' inbox.Trigger
//                let patches = diff(currentTree, tree)
//                patch(rootNode, patches) |> ignore
//                return! loop {state with AppState = {state.AppState with Model = model'}; CurrentTree = Some tree}
//            | _ -> failwith "Shouldn't happen"
//        }
//    loop {AppState = app; Node = None; CurrentTree = None}

open VDom
let view m handler =
    div
        [
            Html.Style {border = "1px solid red"}
        ]
        [
            div [Html.Style { border = "1px solid blue"}; Html.mouseClick (fun x -> window.alert((x.screenX, x.screenY):> obj); handler Increment)] [Html.Text (string "Increment")]
            Html.Text (string m)
            div [Html.Style { border = "1px solid green"}; Html.mouseClick (fun x -> window.alert((x.screenX, x.screenY):> obj); handler Decrement)] [Html.Text (string "Decrement")]
        ]

let update msg model =
    match msg with
    | Increment -> model + 1
    | Decrement -> model - 1

start {Model = 0; View = view; Update = update}

//let hello (model) =
//    VDom.div
//        [
//            Html.Style {border = "1px solid red"}
//            Html.Handler ("onclick",(fun() -> window.alert("Clicked") |> ignore))
//        ]
//        [Html.Text (string model)]
//    |> VDom.render

//  h("div", createObj [ "style" ==> { border = "1px solid red" } ], [| String count |])

//let mutable tree = hello 45
//let mutable rootNode= createElement tree
//document.body.appendChild(rootNode)
//
//let newTree = hello 49
//let patches = diff(tree, newTree)
//patch(rootNode, patches)
//
//let mutable cnt = 42
//let counter() =
//    cnt <- cnt + 1
//    let newTree = hello(cnt)
//    let patches = diff(tree, newTree)
//    rootNode <- patch(rootNode, patches)
//    tree <- newTree
//
//window.setInterval (counter,1000)
//


(*
// 1: Create a function that declares what the DOM should look like
function render(count)  {
    return h('div', {
        style: {
            textAlign: 'center',
            lineHeight: (100 + count) + 'px',
            border: '1px solid red',
            width: (100 + count) + 'px',
            height: (100 + count) + 'px'
        }
    }, [String(count)]);
}

// 2: Initialise the document
var count = 0;      // We need some app data. Here we just store a count.

var tree = render(count);               // We need an initial tree
var rootNode = createElement(tree);     // Create an initial root DOM node ...
document.body.appendChild(rootNode);    // ... and it should be in the document

// 3: Wire up the update logic
setInterval(function () {
      count++;

      var newTree = render(count);
      var patches = diff(tree, newTree);
      rootNode = patch(rootNode, patches);
      tree = newTree;
}, 1000);
*)
