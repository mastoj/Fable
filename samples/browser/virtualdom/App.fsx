#load "html.fsx"
#load "VDom.fsx"
#load "MailboxProcessor.fs"
#r "node_modules/fable-core/Fable.Core.dll"

open Html
open MailboxProcessor
open VDom
open Fable.Core
open Fable.Import.Browser


[<AutoOpen>]
module App =
    type Observer<'T>(next, error, completed) =
        interface System.IObserver<'T> with
            member x.OnCompleted() = completed()
            member x.OnError(e) = error e
            member x.OnNext(v) = next v

    type AppState<'TModel, 'TMessage> = {
            Model: 'TModel
            View: 'TModel -> ('TMessage -> unit) -> Html.Types.Node
            Update: 'TMessage -> 'TModel -> ('TModel * ((unit -> unit) list)) }


    type AppEvents<'TMessage, 'TModel> =
        | ModelChanged of 'TModel*'TModel
        | ActionReceived of 'TMessage

    type Subscriber<'TMessage, 'TModel> = AppEvents<'TMessage, 'TModel> -> unit

    type App<'TModel, 'TMessage> =
        {
            AppState: AppState<'TModel, 'TMessage>
            Node: Node option
            CurrentTree: obj option
            Subscribers: Map<string, Subscriber<'TMessage, 'TModel>>
            NodeSelector: string option
        }

    let createApp appState =
        {
            AppState = appState
            Node = None
            CurrentTree = None
            Subscribers = Map.empty
            NodeSelector = None
        }

    let withStartNode selector app = { app with NodeSelector = Some selector }
    let withSubscriber subscriberId subscriber app =
        let subsribers = app.Subscribers |> Map.add subscriberId subscriber
        { app with Subscribers = subsribers }

    type AppMessage<'TMessage> =
        | AddSubscriber of string*Subscriber<'TMessage, 'TMessage>
        | RemoveSubscriber of string
        | Message of 'TMessage

    let start app =
        let createTree view model handler =
            view model handler
            |> VDom.render

        let startElem =
            match app.NodeSelector with
            | None -> document.body
            | Some sel -> document.body.querySelector(sel) :?> HTMLElement

        MailboxProcessor.Start(fun inbox ->
            let post message =
                inbox.Post (Message message)

            let notifySubscribers subs model =
                subs |> Map.iter (fun key handler -> handler model)

            let rec loop state =
                async {
                    match state.Node, state.CurrentTree with
                    | None,_ ->
                        let tree = createTree state.AppState.View state.AppState.Model post
                        let rootNode = createElement tree
                        startElem.appendChild(rootNode) |> ignore
                        return! loop {state with CurrentTree = Some tree; Node = Some rootNode}
                    | Some rootNode, Some currentTree ->
                        let! message = inbox.Receive()
                        match message with
                        | Message m ->
                            ActionReceived m |> (notifySubscribers state.Subscribers)
                            let (model', jsCalls) = state.AppState.Update m state.AppState.Model
                            let tree = createTree state.AppState.View model' post
                            let patches = diff(currentTree, tree)
                            notifySubscribers state.Subscribers (ModelChanged (model', state.AppState.Model))
                            patch(rootNode, patches) |> ignore
                            jsCalls |> List.iter (fun i -> i())
                            return! loop {state with AppState = {state.AppState with Model = model'}; CurrentTree = Some tree}
                        | _ -> return! loop state
                    | _ -> failwith "Shouldn't happen"
                }
            loop app)
