type MouseEvent =
    {
        altKey: bool
        screenX: int
        screenY: int
    }

type MouseEventHandler =
    | OnClick of (MouseEvent -> unit)

type EventHandler =
    | MouseEventHandler of MouseEventHandler

type Style = (string*string) []

type KeyValue = string*string

type Attribute =
| EventHandler of EventHandler
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
let mouseClick f =
    EventHandler (MouseEventHandler (OnClick f))
