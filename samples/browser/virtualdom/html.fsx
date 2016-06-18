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
        }

    type MouseEventHandler = string*(MouseEvent -> unit)
    type KeyboardEventHandler = string*(KeyboardEvent -> unit)
    type EventHandler = string*(obj -> unit)

    type EventHandlerBinding =
        | MouseEventHandler of MouseEventHandler
        | KeyboardEventHandler of KeyboardEventHandler
        | EventHandler of EventHandler

    type Style = (string*string) list

    type KeyValue = string*string

    type Attribute =
    | EventHandlerBinding of EventHandlerBinding
    | Style of Style
    | KeyValue of KeyValue

    type Element = string * Attribute list
    /// A Node in Html have the following forms
    type VoidElement = string * Attribute list
    type Node =
    /// A regular html element that can contain a list of other nodes
    | Element of Element * Node list
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
    let attribute key value = Attribute.KeyValue (key,value)

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
    //onclick	MouseEvent	DOM L3	A pointing device button has been pressed and released on an element.
    //oncontextmenu	MouseEvent	HTML5	The right button of the mouse is clicked (before the context menu is displayed).
    //ondblclick	MouseEvent	DOM L3	A pointing device button is clicked twice on an element.
    //onmousedown	MouseEvent	DOM L3	A pointing device button (usually a mouse) is pressed on an element.
    //onmouseenter	MouseEvent	DOM L3	A pointing device is moved onto the element that has the listener attached.
    //onmouseleave	MouseEvent	DOM L3	A pointing device is moved off the element that has the listener attached.
    //onmousemove	MouseEvent	DOM L3	A pointing device is moved over an element.
    //onmouseout	MouseEvent	DOM L3	A pointing device is moved off the element that has the listener attached or off one of its children.
    //onmouseover	MouseEvent	DOM L3	A pointing device is moved onto the element that has the listener attached or onto one of its children.
    //onmouseup	MouseEvent	DOM L3	A pointing device button is released over an element.
    //onshow	MouseEvent	HTML5	A contextmenu event was fired on/bubbled to an element that has a contextmenu attribute


    //keydown	KeyboardEvent	DOM L3	A key is pressed down.
    //keypress	KeyboardEvent	DOM L3	A key is pressed down and that key normally produces a character value (use input instead).
    //keyup	KeyboardEvent	DOM L3	A key is released.
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


// List of events from: https://developer.mozilla.org/en-US/docs/Web/Events
//animationend	AnimationEvent	CSS Animations	A CSS animation has completed.
//animationiteration	AnimationEvent	CSS Animations	A CSS animation is repeated.
//animationstart	AnimationEvent	CSS Animations	A CSS animation has started.
//audioprocess	AudioProcessingEvent	Web Audio API	The input buffer of a ScriptProcessorNode is ready to be processed.
//beforeunload	BeforeUnloadEvent	HTML5
//copy	ClipboardEvent	Clipboard	The text selection has been added to the clipboard.
//cut	ClipboardEvent	Clipboard	The text selection has been removed from the document and added to the clipboard.
//paste	ClipboardEvent	Clipboard	Data has been transferred from the system clipboard to the document.
//compositionend	CompositionEvent	DOM L3	The composition of a passage of text has been completed or canceled.
//compositionstart	CompositionEvent	DOM L3	The composition of a passage of text is prepared (similar to keydown for a keyboard input, but works with other inputs such as speech recognition).
//compositionupdate	CompositionEvent	DOM L3	A character is added to a passage of text being composed.
//devicelight	DeviceLightEvent	Ambient Light Events	Fresh data is available from a light sensor.
//devicemotion	DeviceMotionEvent	Device Orientation Events	Fresh data is available from a motion sensor.
//deviceorientation	DeviceOrientationEvent	Device Orientation Events	Fresh data is available from an orientation sensor.
//deviceproximity	DeviceProximityEvent	Proximity Events	Fresh data is available from a proximity sensor (indicates an approximated distance between the device and a nearby object).
//drag	DragEvent	HTML5	An element or text selection is being dragged (every 350ms).
//dragend	DragEvent	HTML5	A drag operation is being ended (by releasing a mouse button or hitting the escape key).
//dragenter	DragEvent	HTML5	A dragged element or text selection enters a valid drop target.
//dragleave	DragEvent	HTML5	A dragged element or text selection leaves a valid drop target.
//dragover	DragEvent	HTML5	An element or text selection is being dragged over a valid drop target (every 350ms).
//dragstart	DragEvent	HTML5	The user starts dragging an element or text selection.
//drop	DragEvent	HTML5	An element is dropped on a valid drop target.
//blur	FocusEvent	DOM L3	An element has lost focus (does not bubble).
//DOMFocusIn  Unimplemented	FocusEvent	DOM L3	An element has received focus (use focus or focusininstead).
//DOMFocusOut  Unimplemented	FocusEvent	DOM L3	An element has lost focus (use blur or focusoutinstead).
//focus	FocusEvent	DOM L3	An element has received focus (does not bubble).
//focusinUnimplemented (see bug 687787)	FocusEvent	DOM L3	An element is about to receive focus (bubbles).
//focusoutUnimplemented (see bug 687787)	FocusEvent	DOM L3	An element is about to lose focus (bubbles).
//gamepadconnected	GamepadEvent	Gamepad	A gamepad has been connected.
//gamepaddisconnected	GamepadEvent	Gamepad	A gamepad has been disconnected.
//hashchange	HashChangeEvent	HTML5	The fragment identifier of the URL has changed (the part of the URL after the #).
//message	MessageEvent	WebSocket	A message is received through a WebSocket.
//message	MessageEvent	Web Workers	A message is received from a Web Worker.
//message	MessageEvent	Web Messaging	A message is received from a child (i)frame or a parent window.
//message	MessageEvent	Server Sent Events	A message is received through an event source.
//DOMAttrModified 	MutationEvent	DOM L3	The value of an attribute has been modified (usemutation observers instead).
//DOMCharacterDataModified 	MutationEvent	DOM L3	A text or another CharacterData has changed (usemutation observers instead).
//DOMNodeInserted 	MutationEvent	DOM L3	A node has been added as a child of another node (usemutation observers instead).
//DOMNodeInsertedIntoDocument 	MutationEvent	DOM L3	A node has been inserted into the document (usemutation observers instead).
//DOMNodeRemoved 	MutationEvent	DOM L3	A node has been removed from its parent node (usemutation observers instead).
//DOMNodeRemovedFromDocument 	MutationEvent	DOM L3	A node has been removed from the document (usemutation observers instead).
//DOMSubtreeModified 	MutationEvent	DOM L3	A change happened in the document (use mutation observers instead).
//DOMAttributeNameChanged 	MutationNameEvent	DOM L3 Removed	The name of an attribute changed (use mutation observers instead).
//DOMElementNameChanged 	MutationNameEvent	DOM L3 Removed	The name of an element changed (use mutation observers instead).
//notificationclick	NotificationEvent	Notifications API	A system notification spawned byServiceWorkerRegistration.showNotification() has been clicked.
//complete	OfflineAudioCompletionEvent	Web Audio API	The rendering of an OfflineAudioContext is terminated.
//pagehide	PageTransitionEvent	HTML5	A session history entry is being traversed from.
//pageshow	PageTransitionEvent	HTML5	A session history entry is being traversed to.
//resourcetimingbufferfull	Performance	Resource Timing	The browser's resource timing buffer is full.
//gotpointercapture	PointerEvent	Pointer Events	Element receives pointer capture.
//lostpointercapture	PointerEvent	Pointer Events	Element lost pointer capture.
//pointercancel	PointerEvent	Pointer Events	The pointer is unlikely to produce any more events.
//pointerdown	PointerEvent	Pointer Events	The pointer enters the active buttons state.
//pointerenter	PointerEvent	Pointer Events	Pointing device is moved inside the hit-testing boundary.
//pointerleave	PointerEvent	Pointer Events	Pointing device is moved out of the hit-testing boundary.
//pointermove	PointerEvent	Pointer Events	The pointer changed coordinates.
//pointerout	PointerEvent	Pointer Events	The pointing device moved out of hit-testing boundary or leaves detectable hover range.
//pointerover	PointerEvent	Pointer Events	The pointing device is moved into the hit-testing boundary.
//pointerup	PointerEvent	Pointer Events	The pointer leaves the active buttons state.
//popstate	PopStateEvent	HTML5	A session history entry is being navigated to (in certain cases).
//abort	ProgressEvent	Progress and XMLHttpRequest	Progression has been terminated (not due to an error).
//error	ProgressEvent	Progress and XMLHttpRequest	Progression has failed.
//load	ProgressEvent	Progress and XMLHttpRequest	Progression has been successful.
//loadend	ProgressEvent	Progress and XMLHttpRequest	Progress has stopped (after "error", "abort" or "load" have been dispatched).
//loadstart	ProgressEvent	Progress and XMLHttpRequest	Progress has begun.
//progress	ProgressEvent	Progress and XMLHttpRequest	In progress.
//progress	ProgressEvent	Offline	The user agent is downloading resources listed by the manifest.
//timeout	ProgressEvent	XMLHttpRequest
//push	PushEvent	Push API	A Service Worker has received a push message.
//pushsubscriptionchange	PushEvent	Push API	A PushSubscription has expired.
//message	ServiceWorkerMessageEventor ExtendableMessageEvent, depending on context.	Service Workers	A message is received from a service worker, or a message is received in a service worker from another context.
//nomatch	SpeechRecognitionEvent	Web Speech API	The speech recognition service returns a final result with no significant recognition.
//result	SpeechRecognitionEvent	Web Speech API	The speech recognition service returns a result — a word or phrase has been positively recognized and this has been communicated back to the app.
//error	SpeechSynthesisErrorEvent	Web Speech API	An error occurs that prevents the utterance from being successfully spoken.
//boundary	SpeechSynthesisEvent	Web Speech API	The spoken utterance reaches a word or sentence boundary
//end	SpeechSynthesisEvent	Web Speech API	The utterance has finished being spoken.
//mark	SpeechSynthesisEvent	Web Speech API	The spoken utterance reaches a named SSML "mark" tag.
//pause	SpeechSynthesisEvent	Web Speech API	The utterance is paused part way through.
//resume	SpeechSynthesisEvent	Web Speech API	A paused utterance is resumed.
//start	SpeechSynthesisEvent	Web Speech API	The utterance has begun to be spoken.
//storage	StorageEvent	Web Storage	A storage area (localStorage or sessionStorage) has changed.
//SVGAbort	SVGEvent	SVG	Page loading has been stopped before the SVG was loaded.
//SVGError	SVGEvent	SVG	An error has occurred before the SVG was loaded.
//SVGLoad	SVGEvent	SVG	An SVG document has been loaded and parsed.
//SVGResize	SVGEvent	SVG	An SVG document is being resized.
//SVGScroll	SVGEvent	SVG	An SVG document is being scrolled.
//SVGUnload	SVGEvent	SVG	An SVG document has been removed from a window or frame.
//SVGZoom	SVGZoomEvent	SVG	An SVG document is being zoomed.
//beginEvent	TimeEvent	SVG	A SMIL animation element begins.
//endEvent	TimeEvent	SVG	A SMIL animation element ends.
//repeatEvent	TimeEvent	SVG	A SMIL animation element is repeated.
//touchcancel	TouchEvent	Touch Events	A touch point has been disrupted in an implementation-specific manners (too many touch points for example).
//touchend	TouchEvent	Touch Events	A touch point is removed from the touch surface.
//touchenter	TouchEvent	Touch Events Removed	A touch point is moved onto the interactive area of an element.
//touchleave	TouchEvent	Touch Events Removed	A touch point is moved off the interactive area of an element.
//touchmove	TouchEvent	Touch Events	A touch point is moved along the touch surface.
//touchstart	TouchEvent	Touch Events	A touch point is placed on the touch surface.
//transitionend	TransitionEvent	CSS Transitions	A CSS transition has completed.
//abort	UIEvent	DOM L3	The loading of a resource has been aborted.
//DOMActivate 	UIEvent	DOM L3	A button, link or state changing element is activated (useclick instead).
//error	UIEvent	DOM L3	A resource failed to load.
//load	UIEvent	DOM L3	A resource and its dependent resources have finished loading.
//resize	UIEvent	DOM L3	The document view has been resized.
//scroll	UIEvent	DOM L3	The document view or an element has been scrolled.
//select	UIEvent	DOM L3	Some text is being selected.
//unload	UIEvent	DOM L3	The document or a dependent resource is being unloaded.
//userproximity	UserProximityEvent	Proximity Events	Fresh data is available from a proximity sensor (indicates whether the nearby object is near the device or not).
//wheel	WheelEvent	DOM L3	A wheel button of a pointing device is rotated in any direction.
//blocked		IndexedDB	An open connection to a database is blocking aversionchange transaction on the same database.
//complete		IndexedDB
//upgradeneeded		IndexedDB	An attempt was made to open a database with a version number higher than its current version. Aversionchange transaction has been created.
//versionchange		IndexedDB	A versionchange transaction completed.
//[<AutoOpen>]
//module App =
//    type AppState<'TModel, 'TMessage> = {
//            Model: 'TModel;
//            View: 'TModel -> ('TMessage -> unit) -> Node;
//            Update: 'TMessage -> 'TModel -> 'TModel}
//
//    type Observer<'T>(next, error, completed) =
//        interface System.IObserver<'T> with
//            member x.OnCompleted() = completed()
//            member x.OnError(e) = error e
//            member x.OnNext(v) = next v
//
//    type App<'TModel, 'TMessage> = {AppState: AppState<'TModel, 'TMessage>; Node: Node option; CurrentTree: obj option}
//
//    let start app =
//        let createTree view model handler =
//            view model handler
//            |> VDom.render
//
//        MailboxProcessor.Start(fun inbox ->
//            let rec loop state =
//                async {
//                    match state.Node, state.CurrentTree with
//                    | None,_ ->
//                        let tree = createTree state.AppState.View state.AppState.Model inbox.Post
//                        let rootNode = createElement tree
//                        document.body.appendChild(rootNode) |> ignore
//                        return! loop {state with CurrentTree = Some tree; Node = Some rootNode}
//                    | Some rootNode, Some currentTree ->
//                        let! message = inbox.Receive()
//                        let model' = state.AppState.Update message state.AppState.Model
//                        let tree = createTree state.AppState.View model' inbox.Post
//                        let patches = diff(currentTree, tree)
//                        patch(rootNode, patches) |> ignore
//                        return! loop {state with AppState = {state.AppState with Model = model'}; CurrentTree = Some tree}
//                    | _ -> failwith "Shouldn't happen"
//                }
//            loop {AppState = app; Node = None; CurrentTree = None})
