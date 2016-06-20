#load "html.fsx"
#load "App.fsx"
#r "node_modules/fable-core/Fable.Core.dll"
open Fable.Core
open Fable.Import.Browser

open App
open Html

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
                    property "placeholder" "What needs to be done?"
                    property "value" model
                    onKeydown (fun x ->
                        printfn "Yolo: %s" x.code
                        if x.code = "Enter"
                        then handler (AddItem {Name = model; Id = 0; Done = false})
                        )
                    onKeyup (fun x -> handler (ChangeInput (x?target?value :?> string))) ]]

let listItem handler item =
    let itemChecked = if item.Done then "true" else ""
    li [ attribute "class" (if item.Done then "completed" else "")]
       [ div [ attribute "class" "view" ]
             [ input [  property "className" "toggle"
                        property "type" "checkbox"
                        property "checked" itemChecked
                        onMouseClick (fun e -> handler (ToggleItem item)) ]
               label [] [ text item.Name ]
               button [ attribute "class" "destroy"
                        onMouseClick (fun e -> handler (Destroy item)) ] [] ]]
     |> (fun i -> printfn "%A" i; i)

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
    | ToggleItem i ->
        let items' =
            model.Items |> List.map (fun i' -> if i' <> i then i' else {i with Done = not i.Done})
        {model with Items = items'}
    | SetActiveFilter f -> { model with Filter = f }
    | ClearCompleted -> { model with Items = model.Items |> List.filter (fun i -> not i.Done)}

start {Model = {Filter = All; Items = [ ]; Input = ""}; View = todoView; Update = todoUpdate}

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
