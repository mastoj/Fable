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
open Fable.Import.Browser


module Html =
//    type Attribute = string * string

    type Handler = unit -> unit
    type Style =
        { border : string }
    type KeyValue = string*string

    type Attribute =
    | Handler of string*Handler
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

let failwithjs() = failwith "JS only"
let [<Import("default","virtual-dom/h")>] h(arg1: string, arg2: obj, arg3: obj[]): obj = failwithjs()
let [<Import("default","virtual-dom/diff")>] diff(arg1:obj, arg2:obj): obj = failwithjs()
let [<Import("default","virtual-dom/patch")>] patch(arg1:obj, arg2:obj): Node = failwithjs()
let [<Import("default","virtual-dom/create-element")>] createElement:obj -> Node = failwithjs()

[<Emit("String($0)")>]
let String i :obj= failwith "JS only"

module VDom =
    open Html

    let rec render node =

        let toAttrs attrs =
            attrs
            |> List.map (function
                    | Handler (evt,handler) -> evt, handler :> obj
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

type AppState<'TModel, 'TMessage> = { Model: 'TModel; View: 'TModel -> ('TMessage -> unit) -> Html.Node; Update: 'TMessage -> 'TModel -> 'TModel}

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

let view m handler =
    VDom.div
        [
            Html.Style {border = "1px solid red"}
            Html.Handler ("onclick",(fun() -> handler Increment))
        ]
        [Html.Text (string m)]

let update msg model =
    match msg with
    | Increment -> model + 1
    | Decrement -> model - 1

let hello (model) =
    VDom.div
        [
            Html.Style {border = "1px solid red"}
            Html.Handler ("onclick",(fun() -> window.alert("Clicked") |> ignore))
        ]
        [Html.Text (string model)]
    |> VDom.render

start {Model = 0; View = view; Update = update}
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
