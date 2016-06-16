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
#load "html.fsx"
#load "App.fsx"
#r "node_modules/fable-core/Fable.Core.dll"
open Fable.Core
open Fable.Import.Browser

open App
open Html

type Item =
    {
        Name: string
        Done: bool
        Id: int
    }
type TodoModel =
    {
        Items: Item list
        Input: string
    }

type Model =
    {
        Counter: int
        Todo: TodoModel
    }

type CounterAction =
    | Increment
    | IncrementWith of int
    | Decrement
    | DecrementWith of int

type TodoAction =
    | AddItem of Item
    | ChangeInput of string
    | MarkAsDone of Item
type Action =
    | CounterAction of CounterAction
    | TodoAction of TodoAction

let todoView m handler =
    let itemList items =
        ul [attribute "className" "todo-list"]
            (items |> List.map (fun i ->
                li [    attribute "className" ("todo-item " + (if i.Done then "done" else ""))
                        onMouseClick (fun x -> handler (MarkAsDone i))] [text i.Name]))

    div
        []
        [
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
        ]

let view m handler =
    div
        [
            Style [|"border","1px solid red"|]
        ]
        [
            div [Style [|"border","1px solid blue"|]; onMouseClick (fun x -> handler (CounterAction Increment)); onDblClick (fun x -> handler (CounterAction (IncrementWith 100)))] [text (string "Increment")]
            text (string m.Counter)
            div [Style [|"border", "1px solid green"; "height", ((string (70+m.Counter)) + "px")|]; onMouseClick (fun x -> handler (CounterAction Decrement)); onDblClick (fun x -> handler (CounterAction (DecrementWith 50)))] [text (string "Decrement")]
            todoView m.Todo (fun x -> handler (Action.TodoAction x))
        ]

let updateTodo msg model =
    match msg with
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

let counterUpdate msg model =
    match msg with
    | Increment -> model + 1
    | IncrementWith x -> model + x
    | Decrement -> model - 1
    | DecrementWith x -> model - 1

let update msg model =
    let model' =
        printfn "Handling: %A" msg
        match msg with
        | CounterAction a -> {model with Counter = (counterUpdate a model.Counter)}
        | TodoAction a -> {model with Todo = (updateTodo a model.Todo)}
    printfn "New model %A" model'
    model'

start {Model = {Counter = 0; Todo = {Items = []; Input = ""}}; View = view; Update = update}
