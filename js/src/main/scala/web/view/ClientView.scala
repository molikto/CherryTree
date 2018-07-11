package web.view

import client.Client
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

  private var theme: ColorScheme = ColorScheme.default

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

  private val mode = p(
    "").render

  private val bottomBar = div(
    width := "100%",
    paddingTop := "1px",
    paddingLeft := "8px",
    fontSize := "14px",
    attr("user-select") := "none",
    alignSelf := "flex-end",
    height := bottomBarSize,
    backgroundColor := theme.bottomBarBackground,
    color := theme.bottomBarText,
    mode
  ).render
  dom.appendChild(bottomBar)

  private val root = div(width := "100%",
    height := "100%",
    padding := "48px",
    color := theme.contentText,
    `class` := "cherrytree-root",
    overflowY := "scroll"
  ).render
  dom.appendChild(root)

  private var previousEditableContent: ContentView = null

  def removeContentEditor() = {
    if (previousEditableContent != null) {
      previousEditableContent.clearMode()
      previousEditableContent = null
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
        if (previousEditableContent != null) removeContentEditor()
        ""
      case Some(mm) => mm match {
        case model.mode.Node.Content(at, aa) =>
          val current = contentAt(at)
          if (current != previousEditableContent) {
            removeContentEditor()
            current.initMode()
          }
          current.syncMode(aa)
          aa match {
            case model.mode.Content.Insertion(_) =>
              "INSERT"
            case model.mode.Content.Visual(_, _) =>
              "VISUAL"
            case model.mode.Content.Normal(_) =>
              "NORMAL"
          }
        case model.mode.Node.Visual(_, _) =>
          if (previousEditableContent != null) removeContentEditor()
          "NODE VISUAL"
      }
    }
  }

  {
    val ClientState(node, selection) = client.state
    def initRenderContentRec(root: model.data.Node, parent: html.Element): Unit = {
      val box = div().render
      parent.appendChild(box)
      val content = root.content match {
        case model.data.Content.Paragraph(cs) =>
          new ParagraphView(cs).dom
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
  }

  event("keydown", (a: KeyboardEvent) => {
    println(s"keydown ${a.key}")
    //a.preventDefault()
  })

  event("keyup", (a: KeyboardEvent) => {
    println(s"keyup ${a.key}")
    //a.preventDefault()
  })

  event("keypress", (a: KeyboardEvent) => {
    println(s"keypress ${a.key}")
    //a.preventDefault()
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
