package web.view

import client.Client
import command.{Commands, Key}
import model.data.{Content, Unicode}
import model.{ClientState, cursor, data, mode}
import monix.execution.{Ack, Scheduler}
import monix.reactive.observers.Subscriber
import org.scalajs.dom.window
import org.scalajs.dom.document
import org.scalajs.dom
import org.scalajs.dom.html
import org.scalajs.dom.html.Div
import org.scalajs.dom.raw._

import scala.collection.mutable.ArrayBuffer
import scala.scalajs.js
import scalatags.JsDom.all._
import monix.execution.Scheduler.Implicits.global

import scala.concurrent.Future
import scala.util.Random

// in this class we use nulls for a various things, but not for public API
class ClientView(private val parent: HTMLElement, val client: Client) extends View {




  var theme: ColorScheme = ColorScheme.default

  dom = div(
    width := "100%",
    `class` := "cherrytree",
    height := "100%",
    display :="flex",
    backgroundColor := theme.contentBackground,
    flexDirection := "column-reverse",
    overflow := "hidden").render
  parent.appendChild(dom)
  defer(_ => parent.removeChild(dom))

  private val bottomBarSize = "24px"


  private val mode = span(
    "").render

  private val debugVersionInfo = span(marginLeft := "12px", "0", onclick := {() =>
    client.change(model.operation.Node.randomTransaction(2, client.state.node, new Random()), None) }).render

  private val debugKeyInfo = span(marginLeft := "12px", "").render

  private val debugErrorInfo = span(color := "red", marginLeft := "12px", "").render

  private val bottomBar = div(
    width := "100%",
    paddingTop := "1px",
    paddingLeft := "8px",
    fontSize := "14px",
    attr("user-select") := "none",
    alignSelf := "flex-end",
    height := bottomBarSize,
    backgroundColor := theme.bottomBarBackground,
    flexDirection := "row",
    color := theme.bottomBarText,
    mode,
    debugVersionInfo,
    debugKeyInfo,
    debugErrorInfo
  ).render
  dom.appendChild(bottomBar)

  private val root = div(width := "100%",
    height := "100%",
    padding := "48px",
    color := theme.contentText,
    `class` := "ct-root",
    overflowY := "scroll"
  ).render
  dom.appendChild(root)

  private val noEditable = div(contenteditable := true, width := "0px", height := "0px").render
  private var currentEditable: HTMLElement = noEditable

  dom.appendChild(noEditable)

  def markEditable(dom: HTMLElement): Unit = {
    if (currentEditable == dom) return
    if (currentEditable != noEditable) {
      throw new IllegalArgumentException("You shouldn't mark editable if other is also editable")
    }
    dom.contentEditable = "true"
    noEditable.contentEditable = "false"
    currentEditable = dom
    currentEditable.focus()
  }

  def unmarkEditableIfEditable(dom: HTMLElement): Unit = {
    if (dom == currentEditable) unmarkEditable(dom)
  }

  def unmarkEditable(dom: HTMLElement): Unit = {
    if (currentEditable == noEditable) return
    if (currentEditable != dom) {
      throw new IllegalArgumentException("You shouldn't unmark editable if you are not the one")
    }
    dom.contentEditable = "false"
    currentEditable = noEditable
    noEditable.contentEditable = "true"
    noEditable.focus()
  }


  private var focusContent: ContentView.General = null

  private def removeFocusContent(): Unit = {
    if (focusContent != null) {
      if (!focusContent.destroyed) focusContent.clearMode()
      focusContent = null
    }
  }

  def childListAt(at: model.cursor.Node): HTMLElement = {
    def rec(a: Node, b: model.cursor.Node): Node = {
      if (b.isEmpty) a
      // ul, li, content
      else rec(a.childNodes(b.head).childNodes(0).childNodes(1), b.tail)
    }
    rec(root.childNodes(0).childNodes(1), at).asInstanceOf[HTMLElement]
  }

  def contentAt(at: model.cursor.Node): ContentView.General = {
    def rec(a: Node, b: model.cursor.Node): Node = {
      if (b.isEmpty) a.childNodes(0)
        // ul, li, content
      else rec(a.childNodes(1).childNodes(b.head).childNodes(0), b.tail)
    }
    View.fromDom[ContentView.General](rec(root.childNodes(0), at))
  }


  def updateMode(m: Option[model.mode.Node], viewUpdated: Boolean): Unit = {

    mode.textContent = m match {
      case None =>
        removeFocusContent()
        ""
      case Some(mm) => mm match {
        case model.mode.Node.Content(at, aa) =>
          val current = contentAt(at)
          if (current != focusContent) {
            removeFocusContent()
            current.initMode()
            focusContent = current
          }
          window.console.log(at.toString())
          window.console.log(current.dom)
          aa match {
            case r: model.mode.Content.Rich =>
              current.asInstanceOf[RichView].updateMode(r, viewUpdated)
            case c: model.mode.Content.Code =>
              current.asInstanceOf[CodeView].updateMode(c, viewUpdated)
          }
          aa match {
            case model.mode.Content.RichInsert(_) =>
              "INSERT"
            case model.mode.Content.RichVisual(_, _) =>
              "VISUAL"
            case model.mode.Content.RichNormal(_) =>
              "NORMAL"
            case model.mode.Content.CodeNormal =>
              "CODE NORMAL"
            case model.mode.Content.CodeInside =>
              "CODE INSIDE"
          }
        case model.mode.Node.Visual(_, _) =>
          if (focusContent != null) removeFocusContent()
          "NODE VISUAL"
      }
    }
  }


  {
    val ClientState(node, selection) = client.state
    insertNodesRec(node, root)
    updateMode(selection, viewUpdated = false)
  }

  private def insertNodesRec(root: model.data.Node, parent: html.Element): Unit = {
    val box = div().render
    parent.appendChild(box)
    box.appendChild(createContent(root.content).dom)
    val list = ul().render
    box.appendChild(list)
    insertNodes(list, 0, root.childs)
  }

  private def insertNodes(list: HTMLElement, at: Int, contents: Seq[model.data.Node]): Unit = {
    if (at == list.childNodes.length) {
      contents.foreach(a => {
        val item = li().render
        list.appendChild(item)
        insertNodesRec(a, item)
      })
    } else {
      val before = list.childNodes(at)
      contents.foreach(a => {
        val item = li().render
        list.insertBefore(item, before)
        insertNodesRec(a, item)
      })
    }
  }

  private def createContent(c: Content): ContentView.General = {
    c match {
      case model.data.Content.Rich(cs) =>
        new RichView(this, cs).asInstanceOf[ContentView.General]
      case model.data.Content.Code(a, lang) =>
        new CodeView(this, a, lang).asInstanceOf[ContentView.General]
    }
  }

  {
    defer(client.stateUpdates.doOnNext(update => {
      for (t <- update.transaction) {
        t match {
          case model.operation.Node.Content(at, c) =>
            contentAt(at).updateContent(update.root(at).content, c, update.viewUpdated)
          case model.operation.Node.Replace(at, c) =>
            val previousContent = contentAt(at)
            val p = previousContent.dom.parentNode
            val cc = createContent(c)
            p.insertBefore(cc.dom, previousContent.dom)
            previousContent.destroy()
          case model.operation.Node.Delete(r) =>
            // look out for this!!!
            val start = r.start
            r.foreach(_ => contentAt(start).destroy())
          case model.operation.Node.Insert(at, childs) =>
            val root = childListAt(at.dropRight(1))
            insertNodes(root, at.last, childs)
          case model.operation.Node.Move(_, _) => ???
        }
      }
      updateMode(update.mode, update.viewUpdated)
      debugVersionInfo.textContent = client.version.toString
    }).subscribe())

    defer(client.errors.doOnNext {
      case Some(e) => debugErrorInfo.textContent = e.getMessage
      case _ =>
    }.subscribe())
  }


  event("keydown", (event: KeyboardEvent) => {
    var key = KeyMap.get(event.key).orNull
    if (key == null && Key.isUnicodeKey(event.key)) {
      key = Key.Grapheme(model.data.Unicode(event.key))
    }
    if (key == null) key = Key.Unknown(event.key)
    val kk = Key(key, meta = event.metaKey, alt = event.altKey, shift = event.shiftKey, control = event.ctrlKey)
    debugKeyInfo.textContent = System.currentTimeMillis().toString.takeRight(10) + " " + event.key + " " + kk.toString
    if (client.keyDown(kk)) {
      event.preventDefault()
    }
  })

  event("keyup", (event: KeyboardEvent) => {
    //window.console.log(event)
  })

  event("keypress", (event: KeyboardEvent) => {
    //window.console.log(event)
  })




  /***
    *
    *
    * copy paste currently disabled for entire document
    *
    *
    */

  event("copy", (a: ClipboardEvent) => {
    a.preventDefault()
  })

  event("cut", (a: ClipboardEvent) => {
    a.preventDefault()
  })

  event("paste", (a: ClipboardEvent) => {
    a.preventDefault()
  })


  /**
    *
    *
    * mouse event currently disabled for entire document
    *
    */

  event("mousedown", (a: MouseEvent) => {
    a.preventDefault()
  })


  event("mouseup", (a: MouseEvent) => {
    // window.setTimeout(() => window.console.log(window.getSelection()), 1)
  })

  class MouseDown {
  }

  event("contextmenu", (a: MouseEvent) => {
    window.console.log(a)
  })



  /**
    *
    *
    * drag drop currently disabled for entire document
    *
    *
    */

  event("dragstart", (a: DragEvent) => {
    a.preventDefault()
  })

  event("dragend", (a: DragEvent) => {
    a.preventDefault()
  })

  event("dragover", (a: DragEvent) => {
    a.preventDefault()
  })

  event("dragenter", (a: DragEvent) => {
    a.preventDefault()
  })

  event("drop", (a: DragEvent) => {
    a.preventDefault()
  })
}
