namespace Fable.Import.React.fs
open Fable.Core
//open Fable.Import.Browser

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

    let createTree tag attributes children =
        let renderEventHandler (eventType, handler) = eventType, handler

        let renderEventBinding binding =
            match binding with
            | MouseEventHandler (eventType, handler) -> (eventType, handler :> obj)//renderMouseEventHandler mh
            | KeyboardEventHandler (eventType, handler) -> (eventType, handler :> obj)
            | EventHandler (eventType, handler) -> (eventType, handler :> obj)
            | x ->
                printfn "Missing renderer for handler: %A" x
                raise (exn "Missing renderer for handler")
            |> renderEventHandler

        let renderAttributes attributes =
            attributes
            |> List.map (fun (Attribute.Attribute (k,v)) -> k,(v :> obj))
            |> (function
                | [] -> None
                | p -> Some ("attributes", (p |> createObj)))

        let toAttrs attrs =
            let (attributes, others) = attrs |> List.partition (function Attribute _ -> true | _ -> false)
            let renderedAttributes = attributes |> renderAttributes
            let renderedOthers =
                others
                |> List.map (function
                        | EventHandlerBinding binding -> binding |> renderEventBinding
                        | Style style -> "style", ((style |> Array.ofList |> Array.map (fun (k,v) -> k + ":" + v) |> join ";") :> obj)
                        | Property (key, value) -> key,(value :> obj)
                        | Attribute _ -> failwith "Should not happen"
                    )
            match renderedAttributes with
            | Some x -> x::renderedOthers
            | _ -> renderedOthers
            |> createObj

        let hAttrs = attributes |> toAttrs
        let childrenArr = children |> List.toArray
        h(tag, hAttrs, childrenArr)

    let diff tree1 tree2 = diff(tree1, tree2)
    let patch node patches = patch(node, patches)
    let createElement e = createElement(e)

    let rec render node =
        match node with
        | Element((tag,attrs), nodes) -> createTree tag attrs (nodes |> List.map render)
        | VoidElement (tag, attrs) -> createTree tag attrs []
        | Text str -> String str
        | WhiteSpace str -> String str
