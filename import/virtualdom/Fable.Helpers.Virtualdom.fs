module Fable.Helpers.Virtualdom

open Fable.Core

[<Import("h","virtual-dom")>]
let h(arg1: string, arg2: obj, arg3: obj[]): obj = failwith "JS only"

[<Import("diff","virtual-dom")>] 
let diff (tree1:obj) (tree2:obj): obj = failwith "JS only"

[<Import("patch","virtual-dom")>] 
let patch (node:obj) (patches:obj): Fable.Import.Browser.Node = failwith "JS only"

[<Import("create","virtual-dom")>]
let createElement (e:obj): Fable.Import.Browser.Node = failwith "JS only"

module Html =
    [<AutoOpen>]
    module Types =
        type MouseEvent =
            {
                altKey: bool
                screenX: int
                screenY: int
            }
        type KeyboardEvent =
            {
                code: string
                keyCode: int
            }

        type MouseEventHandler = string*(MouseEvent -> unit)
        type KeyboardEventHandler = string*(KeyboardEvent -> unit)
        type EventHandler = string*(obj -> unit)

        type EventHandlerBinding =
            | MouseEventHandler of MouseEventHandler
            | KeyboardEventHandler of KeyboardEventHandler
            | EventHandler of EventHandler

        type Style = (string*string) []

        type KeyValue = string*string

        type Attribute =
        | EventHandlerBinding of EventHandlerBinding
        | Style of Style
        | Property of KeyValue
        | Attribute of KeyValue

        type Element = string * Attribute []
        /// A Node in Html have the following forms
        type VoidElement = string * Attribute []
        type Node =
        /// A regular html element that can contain a list of other nodes
        | Element of Element * Node []
        /// A void element is one that can't have content, like link, br, hr, meta
        /// See: https://dev.w3.org/html5/html-author/#void
        | VoidElement of VoidElement
        /// A text value for a node
        | Text of string
        /// Whitespace for formatting
        | WhiteSpace of string

    [<AutoOpen>]
    module Tags =
        let elem tagName attrs children = Element((tagName, attrs), children)
        let voidElem tagName attrs = VoidElement(tagName, attrs)

        let whiteSpace = WhiteSpace
        let text = Text

        // Elements - list of elements here: https://developer.mozilla.org/en-US/docs/Web/HTML/Element
        // Void elements
        let br = voidElem "br"
        let area = voidElem "area"
        let baseHtml = voidElem "base"
        let col = voidElem "col"
        let embed = voidElem "embed"
        let hr = voidElem "hr"
        let img = voidElem "img"
        let input = voidElem "input"
        let link = voidElem "link"
        let meta = voidElem "meta"
        let param = voidElem "param"
        let source = voidElem "source"
        let track = voidElem "track"
        let wbr = voidElem "wbr"

        // Metadata
        let head = elem "head"
        let style = elem "style"
        let title = elem "title"

        // Content sectioning
        let address = elem "address"
        let article = elem "article"
        let aside = elem "aside"
        let footer = elem "footer"
        let header = elem "header"
        let h1 = elem "h1"
        let h2 = elem "h2"
        let h3 = elem "h3"
        let h4 = elem "h4"
        let h5 = elem "h5"
        let h6 = elem "h6"
        let hgroup = elem "hgroup"
        let nav = elem "nav"

        // Text content
        let dd = elem "dd"
        let div = elem "div"
        let dl = elem "dl"
        let dt = elem "dt"
        let figcaption = elem "figcaption"
        let figure = elem "figure"
        let li = elem "li"
        let main = elem "main"
        let ol = elem "ol"
        let p = elem "p"
        let pre = elem "pre"
        let section = elem "section"
        let ul = elem "ul"

        // Inline text semantics
        let a = elem "a"
        let abbr = elem "abbr"
        let b = elem "b"
        let bdi = elem "bdi"
        let bdo = elem "bdo"
        let cite = elem "cite"
        let code = elem "code"
        let data = elem "data"
        let dfn = elem "dfn"
        let em = elem "em"
        let i = elem "i"
        let kbd = elem "kbd"
        let mark = elem "mark"
        let q = elem "q"
        let rp = elem "rp"
        let rt = elem "rt"
        let rtc = elem "rtc"
        let ruby = elem "ruby"
        let s = elem "s"
        let samp = elem "samp"
        let small = elem "small"
        let span = elem "span"
        let strong = elem "strong"
        let sub = elem "sub"
        let sup = elem "sup"
        let time = elem "time"
        let u = elem "u"
        let var = elem "var"

        // Image and multimedia
        let audio = elem "audio"
        let map = elem "map"
        let video = elem "video"

        // Embedded content
        let objectHtml = elem "object"

        // Demarcasting edits
        let del = elem "del"
        let ins = elem "ins"

        // Table content
        let caption = elem "caption"
        let colgroup = elem "colgroup"
        let table = elem "table"
        let tbody = elem "tbody"
        let td = elem "td"
        let tfoot = elem "tfoot"
        let th = elem "th"
        let thead = elem "thead"
        let tr = elem "tr"

        // Forms
        let button = elem "button"
        let datalist = elem "datalist"
        let fieldset = elem "fieldset"
        let form = elem "form"
        let label = elem "label"
        let legend = elem "legend"
        let meter = elem "meter"
        let optgroup = elem "optgroup"
        let option = elem "option"
        let output = elem "output"
        let progress = elem "progress"
        let select = elem "select"
        let textarea = elem "textarea"

        // Interactive elements
        let details = elem "details"
        let dialog = elem "dialog"
        let menu = elem "menu"
        let menuitem = elem "menuitem"
        let summary = elem "summary"

    [<AutoOpen>]
    module Attributes =
        let attribute key value = Attribute.Attribute (key,value)
        let property key value = Attribute.Property (key,value)

    [<AutoOpen>]
    module Events =
        let onMouseEvent eventType f = EventHandlerBinding (MouseEventHandler (eventType, f))

        let onMouseClick = onMouseEvent "onclick"
        let onContextMenu = onMouseEvent "oncontextmenu"
        let onDblClick = onMouseEvent "ondblclick"
        let onMouseDown = onMouseEvent "onmousedown"
        let onMouseEnter = onMouseEvent "onmouseenter"
        let onMouseLeave = onMouseEvent "onmouseleave"
        let onMouseMove = onMouseEvent "onmousemove"
        let onMouseOut = onMouseEvent "onmouseout"
        let onMouseOver = onMouseEvent "onmouseover"
        let onMouseUp = onMouseEvent "onmouseup"
        let onShow = onMouseEvent "onshow"
        let onKeyboardEvent eventType f = EventHandlerBinding (KeyboardEventHandler (eventType, f))
        let onKeydown = onKeyboardEvent "onkeydown"
        let onKeypress = onKeyboardEvent "onkeypress"
        let onKeyup = onKeyboardEvent "onkeyup"

        let onEvent eventType f = EventHandlerBinding (EventHandler (eventType, f))
        let onAbort = onEvent "onabort"
        let onAfterPrint = onEvent "onafterprint"
        let onAudioEnd = onEvent "onaudioend"
        let onAudioStart = onEvent "onaudiostart"
        let onBeforePrint = onEvent "onbeforeprint"
        let onCached = onEvent "oncached"
        let onCanPlay = onEvent "oncanplay"
        let onCanPlayThrough = onEvent "oncanplaythrough"
        let onChange = onEvent "onchange"
        let onChargingChange = onEvent "onchargingchange"
        let onChargingTimeChange = onEvent "onchargingtimechange"
        let onChecking = onEvent "onchecking"
        let onClose = onEvent "onclose"
        let onDischargingTimeChange = onEvent "ondischargingtimechange"
        let onDOMContentLoaded = onEvent "onDOMContentLoaded"
        let onDownloading = onEvent "ondownloading"
        let onDurationchange = onEvent "ondurationchange"
        let onEmptied = onEvent "onemptied"
        let onEnd = onEvent "onend"
        let onEnded = onEvent "onended"
        let onError = onEvent "onerror"
        let onCullScreenChange = onEvent "onfullscreenchange"
        let onCullScreenError = onEvent "onfullscreenerror"
        let onInput = onEvent "oninput"
        let onInvalid = onEvent "oninvalid"
        let onLanguageChange = onEvent "onlanguagechange"
        let onLevelChange = onEvent "onlevelchange"
        let onLoadedData = onEvent "onloadeddata"
        let onLoadedMetaData = onEvent "onloadedmetadata"
        let onNoUpdate = onEvent "onnoupdate"
        let onObsolete = onEvent "onobsolete"
        let onOffline = onEvent "onoffline"
        let onOnline = onEvent "ononline"
        let onOpen = onEvent "onopen"
        let onOrientationChange = onEvent "onorientationchange"
        let onPause = onEvent "onpause"
        let onPointerlockchange = onEvent "onpointerlockchange"
        let onPointerlockerror = onEvent "onpointerlockerror"
        let onPlay = onEvent "onplay"
        let onPlaying = onEvent "onplaying"
        let onRateChange = onEvent "onratechange"
        let onReadyStateChange = onEvent "onreadystatechange"
        let onReset = onEvent "onreset"
        let onSeeked = onEvent "onseeked"
        let onSeeking = onEvent "onseeking"
        let onSelectStart = onEvent "onselectstart"
        let onSelectionChange = onEvent "onselectionchange"
        let onSoundEnd = onEvent "onsoundend"
        let onSoundStart = onEvent "onsoundstart"
        let onSpeechEnd = onEvent "onspeechend"
        let onSpeechStart = onEvent "onspeechstart"
        let onStalled = onEvent "onstalled"
        let onStart = onEvent "onstart"
        let onSubmit = onEvent "onsubmit"
        let onSuccess = onEvent "onsuccess"
        let onSuspend = onEvent "onsuspend"
        let onTimeUpdate = onEvent "ontimeupdate"
        let onUpdateReady = onEvent "onupdateready"
        let onVoicesChanged = onEvent "onvoiceschanged"
        let onVisibilityChange = onEvent "onvisibilitychange"
        let onVolumeChange = onEvent "onvolumechange"
        let onVrdisplayConnected = onEvent "onvrdisplayconnected"
        let onVrdisplayDisconnected = onEvent "onvrdisplaydisconnected"
        let onVrdisplayPresentChange = onEvent "onvrdisplaypresentchange"
        let onWaiting = onEvent "onwaiting"

        let onBlur = onEvent "onblur"
        let onFocus = onEvent "onfocus"

open Html
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
            View: ('TMessage -> unit) -> 'TModel -> Html.Types.Node
            Update: 'TModel -> 'TMessage -> ('TModel * ((unit -> unit) [])) }


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

    type Renderer =
        {
            Render: Html.Types.Node -> obj
            Diff: obj -> obj -> obj
            Patch: Fable.Import.Browser.Node -> obj -> Fable.Import.Browser.Node
            CreateElement: obj -> Fable.Import.Browser.Node
        }

    let start renderer app =
        let renderTree view handler model =
            view handler model
            |> renderer.Render

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
                        let tree = renderTree state.AppState.View post state.AppState.Model
                        let rootNode = renderer.CreateElement tree
                        startElem.appendChild(rootNode) |> ignore
                        return! loop {state with CurrentTree = Some tree; Node = Some rootNode}
                    | Some rootNode, Some currentTree ->
                        let! message = inbox.Receive()
                        match message with
                        | Message msg ->
                            ActionReceived msg |> (notifySubscribers state.Subscribers)
                            let (model', jsCalls) = state.AppState.Update state.AppState.Model msg
                            let tree = renderTree state.AppState.View post model'
                            let patches = renderer.Diff currentTree tree
                            notifySubscribers state.Subscribers (ModelChanged (model', state.AppState.Model))
                            renderer.Patch rootNode patches |> ignore
                            jsCalls |> Array.iter (fun i -> i())
                            return! loop {state with AppState = {state.AppState with Model = model'}; CurrentTree = Some tree}
                        | _ -> return! loop state
                    | _ -> failwith "Shouldn't happen"
                }
            loop app)

let createTree tag attributes children =
    let renderEventHandler (eventType, handler) = eventType, handler

    let renderEventBinding binding =
        match binding with
        | MouseEventHandler (eventType, handler) -> (eventType, handler :> obj)//renderMouseEventHandler mh
        | KeyboardEventHandler (eventType, handler) -> (eventType, handler :> obj)
        | EventHandler (eventType, handler) -> (eventType, handler :> obj)
        |> renderEventHandler

    let renderAttributes attributes =
        attributes
        |> Array.map (function
                        | Attribute.Attribute (k,v) -> Some (k ==> v)
                        | _ -> None)
        |> Array.choose id
        |> (function
                | [||] -> None
                | p -> Some ("attributes" ==> (p |> createObj)))

    let toAttrs attrs =
        let (attributes, others) = attrs |> Array.partition (function Attribute _ -> true | _ -> false)
        let renderedAttributes = attributes |> renderAttributes
        let renderedOthers =
            others
            |> Array.map (function
                    | EventHandlerBinding binding -> binding |> renderEventBinding
                    | Style style ->
                        let styleObj =
                            style
                            |> Array.map (fun (k,v) -> k ==> v)
                            |> createObj

                        "style" ==> styleObj
                    | Property (key, value) -> key ==> value
                    | Attribute _ -> failwith "Should not happen"
                )
        match renderedAttributes with
        | Some x -> Array.append [|x|] renderedOthers
        | _ -> renderedOthers
        |> createObj

    let hAttrs = attributes |> toAttrs
    h(tag, hAttrs, children)

let rec render node =
    match node with
    | Element((tag,attrs), nodes) -> createTree tag attrs (nodes |> Array.map render)
    | VoidElement (tag, attrs) -> createTree tag attrs [||]
    | Text str -> box(string str)
    | WhiteSpace str -> box(string str)

let renderer =
    {
        Render = render
        Diff = diff
        Patch = patch
        CreateElement = createElement
    }
