package web.view

import client.Client
import command.{Commands, Key}
import model.{ClientState, cursor, data}
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

// in this class we use nulls for a various things, but not for public API
class ClientView(private val parent: HTMLElement, private val client: Client) extends View {

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

  private val debugInfo = span(marginLeft := "12px", "").render

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
    debugInfo
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

  private var previousFocusContent: ContentView = null

  def removeFocusContent() = {
    if (previousFocusContent != null) {
      previousFocusContent.clearMode()
      previousFocusContent = null
    }
  }

  def contentAt(at: model.cursor.Node): ContentView = {
    def rec(a: Node, b: model.cursor.Node): Node = {
      if (b.isEmpty) a.childNodes.item(0)
        // ul, li, content
      else rec(a.childNodes.item(1).childNodes.item(b.head).childNodes.item(0), b.tail)
    }
    View.fromDom[ContentView](rec(root, at).childNodes.item(0))
  }

  def syncMode(m: Option[model.mode.Node]): Unit = {
    mode.textContent = m match {
      case None =>
        if (previousFocusContent != null) removeFocusContent()
        ""
      case Some(mm) => mm match {
        case model.mode.Node.Content(at, aa) =>
          val current = contentAt(at)
          if (current != previousFocusContent) {
            removeFocusContent()
            current.initMode()
          }
          current.updateMode(aa)
          aa match {
            case model.mode.Content.Insertion(_) =>
              "INSERT"
            case model.mode.Content.Visual(_, _) =>
              "VISUAL"
            case model.mode.Content.Normal(_) =>
              "NORMAL"
          }
        case model.mode.Node.Visual(_, _) =>
          if (previousFocusContent != null) removeFocusContent()
          "NODE VISUAL"
      }
    }
  }

  /**
    * initialize
    */
  {
    val ClientState(node, selection) = client.state
    def initRenderContentRec(root: model.data.Node, parent: html.Element): Unit = {
      val box = div().render
      parent.appendChild(box)
      val content = root.content match {
        case model.data.Content.Rich(cs) =>
          new RichView(this, cs).dom
        case model.data.Content.Code(a, lang) =>
          p(s"LANGUAGE: $lang", a.toString).render
      }
      box.appendChild(content)
      val list = ul().render
      box.appendChild(list)
      root.childs.foreach(a => {
        val item = li().render
        list.appendChild(item)
        initRenderContentRec(a, item)
      })
    }
    initRenderContentRec(node, root)

    syncMode(selection)
    defer(client.stateUpdates.doOnNext(update => {
      syncMode(update.mode)
    }).subscribe())

    defer(client.errors.doOnNext {
      case Some(e) => debugInfo.textContent = e.getMessage
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
    debugInfo.textContent = System.currentTimeMillis().toString.takeRight(10) + " " + event.key + " " + kk.toString
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
    a.preventDefault()
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
