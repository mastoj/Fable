module MailboxProcessor

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
