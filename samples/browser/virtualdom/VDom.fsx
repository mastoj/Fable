#r "node_modules/fable-core/Fable.Core.dll"
#load "html.fsx"

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

    let rec render node =
//        let renderMouseEventHandler (eventType, handler) = eventType, (handler :> obj)
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
