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
    | ToggleItem of Item
    | Destroy of Item
    | CheckAll
    | UnCheckAll
type Action =
    | CounterAction of CounterAction
    | TodoAction of TodoAction


    //        <section class="todoapp">
    //        			<header class="header">
    //        				<h1>todos</h1>
    //        				<input class="new-todo" placeholder="What needs to be done?" autofocus="">
    //        			</header>
    //        			<footer class="footer" style="display: block;">
    //        				<span class="todo-count"><strong>2</strong> items left</span>
    //        				<ul class="filters">
    //        					<li>
    //        						<a href="#/" class="selected">All</a>
    //        					</li>
    //        					<li>
    //        						<a href="#/active">Active</a>
    //        					</li>
    //        					<li>
    //        						<a href="#/completed">Completed</a>
    //        					</li>
    //        				</ul>
    //        				<button class="clear-completed" style="display: none;"></button>
    //        			</footer>
    //        		</section>



let todoHeader model handler =
//        			<header class="header">
//        				<h1>todos</h1>
//        				<input class="new-todo" placeholder="What needs to be done?" autofocus="">
//        			</header>
    header
        [attribute "className" "header"]
        [   h1 [] [text "todos"]
            input [ attribute "className" "new-todo"
                    attribute "placeholder" "What needs to be done?"
                    attribute "value" model
                    onKeydown (fun x ->
                        printfn "Yolo: %s" x.code
                        if x.code = "Enter"
                        then handler (AddItem {Name = model; Id = 0; Done = false})
                        )
                    onKeyup (fun x -> handler (ChangeInput (x?target?value :?> string))) ]]

let listItem handler item =
// <li data-id="1466286291319" class=""><div class="view"><input class="toggle" type="checkbox"><label>erwerwe</label><button class="destroy"></button></div></li>
    let itemChecked = if item.Done then "true" else ""
    li []
       [ div [ attribute "className" "view" ]
             [ input [  attribute "className" "toggle"
                        attribute "type" "checkbox"
                        attribute "checked" itemChecked
                        onMouseClick (fun e -> handler (ToggleItem item)) ]
               label [] [ text item.Name ]
               button [ attribute "className" "destroy"
                        onMouseClick (fun e -> handler (Destroy item)) ] [] ]]
     |> (fun i -> printfn "%A" i; i)

let itemList handler items =
    ul [ attribute "className" "todo-list" ]
       (items |> List.map (listItem handler))

let todoMain items handler =
    let allChecked = items |> List.exists (fun i -> not i.Done)
    section [   attribute "className" "main"
                attribute "style" "display: block;" ]
            [   input [ attribute "className" "toggle-all"
                        attribute "type" "checkbox"
                        attribute "checked" (if not allChecked then "true" else "")
                        onMouseClick (fun e ->
                                    if allChecked
                                    then handler CheckAll
                                    else handler UnCheckAll) ]
                label [ attribute "for" "toggle-all" ]
                      [ text "Mark all as complete" ]
                (itemList handler items) ]

let todoView m handler =
    section
        [attribute "className" "todoapp"]
        [
            (todoHeader m.Input handler)
            (todoMain m.Items handler)
        ]

let view m handler =
    div
        [
            Style ["border","1px solid red"]
        ]
        [
            div [   Style ["border","1px solid blue"]
                    onMouseClick (fun x -> handler (CounterAction Increment))
                    onDblClick (fun x -> handler (CounterAction (IncrementWith 100)))] [text (string "Increment")]
            text (string m.Counter)
            div [   Style ["border", "1px solid green"; "height", ((string (70+m.Counter)) + "px")]
                    onMouseClick (fun x -> handler (CounterAction Decrement))
                    onDblClick (fun x -> handler (CounterAction (DecrementWith 50)))]
                [text (string "Decrement")]
            todoView m.Todo (fun x -> handler (Action.TodoAction x))
        ]

let updateTodo msg model =
    let checkAllWith v =
        { model with Items = model.Items |> List.map (fun i -> { i with Done = v })}

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
    | CheckAll -> checkAllWith true
    | UnCheckAll -> checkAllWith false
    | Destroy i -> {model with Items = model.Items |> List.filter (fun i' -> i'.Id <> i.Id)}
    | ToggleItem i ->
        let items' =
            model.Items |> List.map (fun i' -> if i' <> i then i' else {i with Done = not i.Done})
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

start {Model = {Counter = 0; Todo = {Items = [ { Name = "Yolo"; Done = false; Id = 0}]; Input = ""}}; View = view; Update = update}
