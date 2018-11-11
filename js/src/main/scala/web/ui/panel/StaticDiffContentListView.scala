package web.ui.panel

import java.util.UUID

import model.NodeQuickDiff
import model.data.Node
import org.scalajs.dom.raw.HTMLElement
import scalatags.JsDom.all._
import web.ui.doc.{DocFramer, LaTeXMacroCache}
import web.view.View


class StaticDiffContentListView(
  override val onClick: UUID => Unit,
  override val onDoubleClick: UUID => Unit,
  override val latexMacroCache: LaTeXMacroCache
) extends View with NodeQuickDiff with DocFramer {

  dom = div().render
  private val list = dom

  private var listData: Array[model.data.Node] = Array.empty

  protected var headerSize = 0
  def addHeader(a: HTMLElement)= {
    headerSize += 1
    list.insertBefore(a, list.childNodes(0))
  }


  override val docFramerIsSmall: Int = 2

  override val docFramerExtraClass: String = "ct-flat-selectable"


  override val useFoldedIcon: Boolean = true

  def size: Int = list.childNodes.length - headerSize

  protected override def eq(a: Node, b: Node): Boolean = a.content == b.content && a.attributes == b.attributes

  protected def domAt(i: Int) = list.childNodes(i + headerSize).asInstanceOf[HTMLElement]

  protected override def performChange(index: Int, oldData: Node, newData: Node): Unit = {
    updateContentViewInsideFrame(domAt(index), newData)
  }

  protected def onAddViewAndHold(view: HTMLElement, data: Node) = {
  }


  protected override def performAdd(i: Int, data: Node): Unit = {
    //if (model.debug_view) println(s"performing add $i")
    val view = contentViewAndHold(data)
    onAddViewAndHold(view, data)
    list.insertBefore(view, domAt(i))
  }

  protected override def performDelete(i: Int): Unit = {
    //if (model.debug_view) println(s"performing delete $i")
    web.view.removeFromParent(domAt(i))
  }

  def update(newData0: Seq[model.data.Node]): Unit = {
    val newData = newData0.toArray
    diff(listData, newData)
    listData = newData
  }

  def updateFocus(uuid: Option[UUID], list: HTMLElement): Option[Int] = {
    var contains: Option[Int] = None
    (0 until size).foreach(i => {
      val ct = contentViewFromWithHold(domAt(i))
      val cu = uuidOf(ct)
      if (uuid.contains(cu)) {
        contains = Some(i)
        web.view.scrollInToViewIfNotVisible(ct.dom, list, 10, 10, 30, 30)
        ct.dom.classList.add("ct-highlight")
      } else {
        ct.dom.classList.remove("ct-highlight")
      }
    })
    contains
  }
}
