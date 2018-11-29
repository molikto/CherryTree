package web.ui.doc

import java.util.UUID

import model.data.NodeType
import org.scalajs.dom.raw.HTMLElement
import org.scalajs.dom.raw
import org.scalajs.dom.raw._
import scalatags.JsDom.all.{tag, _}
import web.view.View
import web.ui.content.{ContentView, ContentViewCreator}
import web.ui.content.ContentView.General

import scala.scalajs.js

trait DocFramer extends ContentViewCreator {

  val docFramerExtraClass: String = ""

  protected val parentHeadingLevel: Int = -1

  val latexMacroCache: LaTeXMacroCache

  val onClick: UUID => Unit = null
  val onDoubleClick: UUID => Unit = null

  val useFoldedIcon: Boolean = false

  def contentViewFromWithHold(a: raw.Node): ContentView.General = {
    View.fromDom[ContentView.General](a.childNodes(0).childNodes(0))
  }



  def insertExtraToContentView(a: HTMLElement, b: HTMLElement): Unit = {
    val box = a.childNodes(0)
    box.appendChild(b)
  }

  def extraViewInFrame(a: HTMLElement): HTMLElement = {
    a.childNodes(0).childNodes(1).asInstanceOf[HTMLElement]
  }

  def uuidOf(a: ContentView.General): UUID = {
    UUID.fromString(a.dom.asInstanceOf[js.Dynamic].ctUuid.asInstanceOf[String])
  }

  private val onClickListener: js.Function1[Event, _] = (ev: Event) => {
    val uuid = UUID.fromString(ev.currentTarget.asInstanceOf[js.Dynamic].ctUuid.asInstanceOf[String])
    onClick(uuid)
  }

  private val onDoubleClickListener: js.Function1[Event, _] = (ev: Event) => {
    val uuid = UUID.fromString(ev.currentTarget.asInstanceOf[js.Dynamic].ctUuid.asInstanceOf[String])
    onDoubleClick(uuid)
  }

  private def create(a: model.data.Node): General = {
    val cv = contentViewCreate(a.content)
    if (docFramerExtraClass != "") {
      cv.dom.classList.add(docFramerExtraClass)
    }
    if (onClick != null || onDoubleClick != null) {
      cv.dom.asInstanceOf[js.Dynamic].ctUuid = a.uuid.toString
    }
    if (onClick != null) {
      cv.dom.addEventListener("click", onClickListener)
    }
    if (onDoubleClick != null) {
      cv.dom.addEventListener("dblclick", onDoubleClickListener)
    }
    cv
  }

  def updateContentViewInsideFrame(a: HTMLElement, data: model.data.Node): Unit = {
    val oldView = contentViewFromWithHold(a)
    if (contentViewMatches(data.content, oldView)) {
      oldView.updateContent(data.content)
      updateContentViewAndHoldAttribute(a, data)
    } else {
      oldView.destroy()
      val box = a.childNodes(0)
      create(data).attachToNode(box, box.childNodes(0))
    }
  }

  def contentViewAndHold(node: model.data.Node): HTMLElement = {
    div(
      cls := "ct-d-folded",
      display := "flex",
      flexDirection := "row",
      div(
        cls := classesFromNodeAttribute(NodeType.Article, node, node.nodeType.getOrElse(NodeType.Li)),
        create(node)
      ),
      tag("span")(
        cls := (if (useFoldedIcon) "ct-d-hold ct-d-hold-folded " else "ct-d-hold "),
        if (docFramerIsSmall >= 2) {
          marginLeft := "-8px"
          marginTop := "6px"
        } else border := "none"
      )
    ).render
  }

  def updateContentViewAndHoldAttribute(a: HTMLElement, node: model.data.Node): Unit = {
    a.childNodes(0).asInstanceOf[HTMLElement].className =
        classesFromNodeAttribute(NodeType.Article, node, node.nodeType.getOrElse(NodeType.Li))
  }


}
