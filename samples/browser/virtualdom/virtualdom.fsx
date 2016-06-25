(**
 - title: D3 world tour
 - tagline: Looping through countries of the world
 - app-style: height:500px; width:500px; margin:10px auto 10px auto;
 - require-paths: `'d3':'https://d3js.org/d3.v3.min','queue': 'https://d3js.org/queue.v1.min','topojson': 'https://d3js.org/topojson.v1.min' `
 - intro: This demo is a Fable port of [Mike Bostock's World Tour](http://bl.ocks.org/mbostock/4183330)
   D3 demo. It uses the D3 library to create a visualization that loops through all countries of
   the world and shows them on the globe one by one.
   You can find the [full source code on GitHub](https://github.com/fsprojects/Fable/blob/master/samples/browser/ap/ap.fsx).

   On the technical side, the demo shows some of the more interesting aspects of
   calling JavaScript libraries from Fable. You'll learn how to define mappings for
   imported scripts, how to pass lambdas to JS code and the `?` operator.
*)
(*** hide ***)
#load "Fable.Helpers.Virtualdom.fsx"
#load "Fable.Import.Virtualdom.fsx"
#r "node_modules/fable-core/Fable.Core.dll"
(**
Simple couter example
---------------------

The first application sample using the said architecture is a simple counter.

*)

(**
    (*** raw ***)
    <div id="thisisraw">This is raw</div>
*)
open Fable.Core
open Fable.Import
open Fable.Import.Browser

open Fable.Helpers.Virtualdom.App
open Fable.Helpers.Virtualdom.Html
open Fable.Import.Virtualdom.Virtualdom

// Storage
module Storage =
    let private STORAGE_KEY = "vdom-storage"
    open Microsoft.FSharp.Core
    let fetch<'T> (): 'T [] =
        Browser.localStorage.getItem(STORAGE_KEY)
        |> function null -> "[]" | x -> unbox x
        |> JS.JSON.parse |> unbox

    let save<'T> (todos: 'T []) =
        Browser.localStorage.setItem(STORAGE_KEY, JS.JSON.stringify todos)

// Todo model
type Filter =
    | All
    | Completed
    | Active

type Item =
    {
        Name: string
        Done: bool
        Id: int
        IsEditing: bool
    }
type TodoModel =
    {
        Items: Item list
        Input: string
        Filter: Filter
    }

type TodoAction =
    | AddItem of Item
    | ChangeInput of string
    | MarkAsDone of Item
    | ToggleItem of Item
    | Destroy of Item
    | CheckAll
    | UnCheckAll
    | SetActiveFilter of Filter
    | ClearCompleted
    | EditItem of Item
    | SaveItem of Item*string

// Todo view
let filterToTextAndUrl = function
    | All -> "All", ""
    | Completed -> "Completed", "completed"
    | Active -> "Active", "active"

let filter handler activeFilter f =
    let linkClass = if f = activeFilter then "selected" else ""
    let fText,url = f |> filterToTextAndUrl
    li
        [ onMouseClick (fun _ -> SetActiveFilter f |> handler)]
        [ a
            [ attribute "href" ("#/" + url); attribute "class" linkClass ]
            [ text fText] ]

let filters model handler =
    ul
        [ attribute "class" "filters" ]
        ([ All; Active; Completed ] |> List.map (filter handler model.Filter))

let todoFooter model handler =
    let clearVisibility =
        if model.Items |> List.exists (fun i -> i.Done)
        then ""
        else "none"
    let activeCount = model.Items |> List.filter (fun i -> not i.Done) |> List.length |> string
    footer
        [   attribute "class" "footer"; Style ["display","block"]]
        [   span
                [ attribute "class" "todo-count" ]
                [   strong [] [text activeCount]
                    text " items left" ]
            (filters model handler)
            button
                [   attribute "class" "clear-completed"
                    Style [ "display", clearVisibility ]
                    onMouseClick (fun _ -> handler ClearCompleted)]
                [ text "Clear completed" ] ]


let todoHeader model handler =
    header
        [attribute "class" "header"]
        [   h1 [] [text "todos"]
            input [ attribute "class" "new-todo"
                    property "placeholder" "What needs to be done??"
                    property "value" model
                    onKeydown (fun x ->
                        if x.code = "Enter"
                        then handler (AddItem {Name = model; Id = 0; Done = false; IsEditing = false})
                        )
                    onKeyup (fun x -> handler (ChangeInput (x?target?value :?> string))) ]]

let listItem handler item =
    let itemChecked = if item.Done then "true" else ""
    let editClass = if item.IsEditing then "editing" else ""
    li [ attribute "class" ((if item.Done then "completed " else " ") + editClass)]
       [ div [  attribute "class" "view"
                onDblClick (fun x -> printfn "%A" x; EditItem item |> handler) ]
             [ input [  property "className" "toggle"
                        property "type" "checkbox"
                        property "checked" itemChecked
                        onMouseClick (fun e -> handler (ToggleItem item)) ]
               label [] [ text item.Name ]
               button [ attribute "class" "destroy"
                        onMouseClick (fun e -> handler (Destroy item)) ] [] ]
         input [ attribute "class" "edit"
                 attribute "value" item.Name
                 property "id" ("item-"+item.Id.ToString())
                 onBlur (fun e -> SaveItem (item, (e?target?value :?> string)) |> handler) ] ]

let itemList handler items activeFilter =
    let filterItems i =
        match activeFilter with
        | All -> true
        | Completed -> i.Done
        | Active -> not i.Done

    ul [attribute "class" "todo-list" ]
       (items |> List.filter filterItems |> List.map (listItem handler))

let todoMain model handler =
    let items = model.Items
    let allChecked = items |> List.exists (fun i -> not i.Done)
    section [  attribute "class" "main"
               Style [ "style", "block" ] ]
            [   input [ property "id" "toggle-all"
                        attribute "class" "toggle-all"
                        property "type" "checkbox"
                        property "checked" (if not allChecked then "true" else "")
                        onMouseClick (fun e ->
                                    if allChecked
                                    then handler CheckAll
                                    else handler UnCheckAll) ]
                label [ attribute "for" "toggle-all" ]
                      [ text "Mark all as complete" ]
                (itemList handler items model.Filter) ]

let todoView m handler =
    section
        [attribute "class" "todoapp"]
        ((todoHeader m.Input handler)::(if m.Items |> List.isEmpty
                then []
                else [  (todoMain m handler)
                        (todoFooter m handler) ] ))

// Todo update
let todoUpdate msg model =
    let checkAllWith v =
        { model with Items = model.Items |> List.map (fun i -> { i with Done = v })}

    let updateItem i model =
        let items' =
            model.Items |> List.map (fun i' -> if i'.Id <> i.Id then i' else i)
        {model with Items = items'}

    let model' =
        match msg with
        | AddItem item ->
            let maxId =
                if model.Items |> List.isEmpty then 1
                else
                    model.Items
                    |> List.map (fun x -> x.Id)
                    |> List.max
            let item' = {item with Id = maxId + 1}
            {model with Items = item'::model.Items; Input = ""}
        | ChangeInput v -> {model with Input = v}
        | MarkAsDone i ->
            let items' =
                model.Items |> List.map (fun i' -> if i' <> i then i' else {i with Done = true})
            {model with Items = items'}
        | CheckAll -> checkAllWith true
        | UnCheckAll -> checkAllWith false
        | Destroy i -> {model with Items = model.Items |> List.filter (fun i' -> i'.Id <> i.Id)}
        | ToggleItem i -> updateItem {i with Done = not i.Done} model
        | SetActiveFilter f -> { model with Filter = f }
        | ClearCompleted -> { model with Items = model.Items |> List.filter (fun i -> not i.Done)}
        | EditItem i -> updateItem { i with IsEditing = true} model
        | SaveItem (i,str) -> updateItem { i with Name = str; IsEditing = false} model
    let jsCalls =
        match msg with
        | EditItem i -> [fun () -> document.getElementById("item-" + (i.Id.ToString())).focus()]
        | _ -> []
    model',jsCalls

open Storage
let initList = fetch<Item>() |> List.ofArray
let initModel = {Filter = All; Items = initList; Input = ""}

let renderer =
    {
        Render = render
        Diff = diff
        Patch = patch
        CreateElement = createElement
    }

let app =
    createApp {Model = initModel; View = todoView; Update = todoUpdate}
    |> (withSubscriber "storagesub" (function (ModelChanged (newModel,old)) -> save (newModel.Items |> Array.ofList) | _ -> ()))
    |> (withSubscriber "modellogger" (printfn "%A"))
app |> start renderer

//type Model =
//    {
//        Counter: int
//        Todo: TodoModel
//    }
//
//type CounterAction =
//    | Increment
//    | IncrementWith of int
//    | Decrement
//    | DecrementWith of int
//
//type Action =
//    | CounterAction of CounterAction
//    | TodoAction of TodoAction
//
//let counterView m handler =
//    div []
//        [
//            div [   Style ["border","1px solid blue"]
//                    onMouseClick (fun x -> handler (Increment))
//                    onDblClick (fun x -> handler ((IncrementWith 100)))] [text (string "Increment")]
//            text (string m)
//            div [   Style ["border", "1px solid green"; "height", ((string (70+m)) + "px")]
//                    onMouseClick (fun x -> handler (Decrement))
//                    onDblClick (fun x -> handler (DecrementWith 50))]
//                [text (string "Decrement")]
//        ]
//
//
//let view m handler =
//    div
//        [
//            Style ["border","1px solid red"]
//        ]
//        [
//            counterView m.Counter (Action.CounterAction >> handler)
//            todoView m.Todo (Action.TodoAction >> handler)
//        ]
//
//let counterUpdate msg model =
//    match msg with
//    | Increment -> model + 1
//    | IncrementWith x -> model + x
//    | Decrement -> model - 1
//    | DecrementWith x -> model - 1
//
//let update msg model =
//    let model' =
//        printfn "Handling: %A" msg
//        match msg with
//        | CounterAction a -> {model with Counter = (counterUpdate a model.Counter)}
//        | TodoAction a -> {model with Todo = (todoUpdate a model.Todo)}
//    printfn "New model %A" model'
//    model'
//
//start {Model = {Counter = 0; Todo = {Filter = All; Items = [ { Name = "Yolo"; Done = false; Id = 0}]; Input = ""}}; View = view; Update = update}
