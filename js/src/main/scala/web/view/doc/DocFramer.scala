package web.view.doc

import org.scalajs.dom.raw.HTMLElement
import client.Client
import _root_.doc.DocTransaction
import client.Client.ViewMessage
import model.cursor
import model.cursor.Node
import org.scalajs.dom.raw._
import scalatags.JsDom.all.{tag, _}
import settings.Settings
import web.view.View
import web.view.content.ContentView
import web.view.content.ContentView.General

import scala.scalajs.js

trait DocFramer {

  val docFramerIsSmall: Int = 0
  val docFramerExtraClass: String = ""

  val onClick: String => Unit = null

  val useFoldedIcon: Boolean = false

  def contentViewFromWithHold(a: HTMLElement): ContentView.General = {
    View.fromDom[ContentView.General](a.childNodes(0).childNodes(0))
  }

  def insertExtraToContentView(a: HTMLElement, b: HTMLElement): Unit = {
    val box = a.childNodes(0)
    box.appendChild(b)
  }

  def extraViewInFrame(a: HTMLElement): HTMLElement = {
    a.childNodes(0).childNodes(1).asInstanceOf[HTMLElement]
  }

  private val onClickListener: js.Function1[Event, _] = (ev: Event) => {
    val uuid = ev.currentTarget.asInstanceOf[js.Dynamic].ctUuid.asInstanceOf[String]
    onClick(uuid)
  }

  private def create(a: model.data.Node): General = {
    val cv = ContentView.create(a.content)
    if (docFramerExtraClass != "") {
      cv.dom.classList.add(docFramerExtraClass)
    }
    if (onClick != null) {
      cv.dom.asInstanceOf[js.Dynamic].ctUuid = a.uuid
      cv.dom.addEventListener("click", onClickListener)
    }
    cv
  }

  def updateContentViewInsideFrame(a: HTMLElement, data: model.data.Node): Unit = {
    val oldView = contentViewFromWithHold(a)
    if (ContentView.matches(data.content, oldView)) {
      oldView.updateContent(data.content)
    } else {
      oldView.destroy()
      val box = a.childNodes(0)
      create(data).attachToNode(box, box.childNodes(0))
    }
  }

  def contentViewAndHold(node: model.data.Node): HTMLElement = {
    div(
      `class` := "ct-d-folded",
      display := "flex",
      flexDirection := "row",
      div(
        `class` := classesFromNodeAttribute(node),
        create(node)
      ),
      tag("i")(`class` := (if (useFoldedIcon) "ct-d-hold ct-d-hold-folded" else "ct-d-hold"))
    ).render
  }

  def updateContentViewAndHoldAttribute(a: HTMLElement, node: model.data.Node) = {
    a.childNodes(0).asInstanceOf[HTMLElement].className = classesFromNodeAttribute(node)
  }

  def classesFromNodeAttribute(node: model.data.Node): String = {
    "ct-d-box " + (node.attribute(model.data.Node.ContentType).map {
      case model.data.Node.ContentType.Cite => "ct-d-cite"
      case model.data.Node.ContentType.Br => "ct-d-br"
      case model.data.Node.ContentType.Heading(j) =>
        if (docFramerIsSmall == 0) {
          if (j > 1) s"ct-d-heading ct-d-h$j" else s"ct-d-h1"
        } else if (docFramerIsSmall == 1) {
          if (j > 1) s"ct-d-heading ct-d-hs${if (j >= 4) "s" else j.toString}" else s"ct-d-hs1"
        } else {
          if (j > 1) s"ct-d-heading" else s"ct-d-h1"
        }
      case _ => ""
    }.getOrElse("") + " " + node.attribute(model.data.Node.ChildrenType).map {
      case model.data.Node.ChildrenType.UnorderedList => "ct-d-ul"
      case model.data.Node.ChildrenType.OrderedList => "ct-d-ol"
      case model.data.Node.ChildrenType.Paragraphs => "ct-d-ps"
      case model.data.Node.ChildrenType.DashList => "ct-d-dl"
      case _ => ""
    }.getOrElse(""))
  }
}
