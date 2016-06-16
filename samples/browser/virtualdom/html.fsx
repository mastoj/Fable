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

type Style = (string*string) []

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

let private createElem tagName attrs children = Element((tagName, attrs), children)
let private createVoidElem tagName attrs = VoidElement(tagName, attrs)

// Elements - list of elements here: https://developer.mozilla.org/en-US/docs/Web/HTML/Element
// Void elements
let br = createVoidElem "br"
let area = createVoidElem "area"
let baseHtml = createVoidElem "base"
let col = createVoidElem "col"
let embed = createVoidElem "embed"
let hr = createVoidElem "hr"
let img = createVoidElem "img"
let input = createVoidElem "input"
let link = createVoidElem "link"
let meta = createVoidElem "meta"
let param = createVoidElem "param"
let source = createVoidElem "source"
let track = createVoidElem "track"
let wbr = createVoidElem "wbr"

// Metadata
let head = createElem "head"
let style = createElem "style"
let title = createElem "title"

// Content sectioning
let address = createElem "address"
let article = createElem "article"
let aside = createElem "aside"
let footer = createElem "footer"
let header = createElem "header"
let h1 = createElem "h1"
let h2 = createElem "h2"
let h3 = createElem "h3"
let h4 = createElem "h4"
let h5 = createElem "h5"
let h6 = createElem "h6"
let hgroup = createElem "hgroup"
let nav = createElem "nav"

// Text content
let dd = createElem "dd"
let div = createElem "div"
let dl = createElem "dl"
let dt = createElem "dt"
let figcaption = createElem "figcaption"
let figure = createElem "figure"
let li = createElem "li"
let main = createElem "main"
let ol = createElem "ol"
let p = createElem "p"
let pre = createElem "pre"
let ul = createElem "ul"

// Inline text semantics
let a = createElem "a"
let abbr = createElem "abbr"
let b = createElem "b"
let bdi = createElem "bdi"
let bdo = createElem "bdo"
let cite = createElem "cite"
let code = createElem "code"
let data = createElem "data"
let dfn = createElem "dfn"
let em = createElem "em"
let i = createElem "i"
let kbd = createElem "kbd"
let mark = createElem "mark"
let q = createElem "q"
let rp = createElem "rp"
let rt = createElem "rt"
let rtc = createElem "rtc"
let ruby = createElem "ruby"
let s = createElem "s"
let samp = createElem "samp"
let small = createElem "small"
let span = createElem "span"
let strong = createElem "strong"
let sub = createElem "sub"
let sup = createElem "sup"
let time = createElem "time"
let u = createElem "u"
let var = createElem "var"

// Image and multimedia
let audio = createElem "audio"
let map = createElem "map"
let video = createElem "video"

// Embedded content
let objectHtml = createElem "object"

// Demarcasting edits
let del = createElem "del"
let ins = createElem "ins"

// Table content
let caption = createElem "caption"
let colgroup = createElem "colgroup"
let table = createElem "table"
let tbody = createElem "tbody"
let td = createElem "td"
let tfoot = createElem "tfoot"
let th = createElem "th"
let thead = createElem "thead"
let tr = createElem "tr"

// Forms
let button = createElem "button"
let datalist = createElem "datalist"
let fieldset = createElem "fieldset"
let form = createElem "form"
let label = createElem "label"
let legend = createElem "legend"
let meter = createElem "meter"
let optgroup = createElem "optgroup"
let option = createElem "option"
let output = createElem "output"
let progress = createElem "progress"
let select = createElem "select"
let textarea = createElem "textarea"

// Interactive elements
let details = createElem "details"
let dialog = createElem "dialog"
let menu = createElem "menu"
let menuitem = createElem "menuitem"
let summary = createElem "summary"

let text s = Text s

let attribute key value = Attribute.KeyValue (key,value)

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
