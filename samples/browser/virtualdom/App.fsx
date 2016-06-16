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
    type AppState<'TModel, 'TMessage> = {
            Model: 'TModel
            View: 'TModel -> ('TMessage -> unit) -> Html.Types.Node
            Update: 'TMessage -> 'TModel -> 'TModel }

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
                        let model' = state.AppState.Update message state.AppState.Model
                        let tree = createTree state.AppState.View model' inbox.Post
                        let patches = diff(currentTree, tree)
                        patch(rootNode, patches) |> ignore
                        return! loop {state with AppState = {state.AppState with Model = model'}; CurrentTree = Some tree}
                    | _ -> failwith "Shouldn't happen"
                }
            loop {AppState = app; Node = None; CurrentTree = None})
