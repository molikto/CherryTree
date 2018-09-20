package web.ui

import model.NodeQuickDiff
import model.data.{Content, Node}
import org.scalajs.dom.raw.HTMLElement
import scalatags.JsDom.all._
import util.QuickDiff
import web.ui.content.ContentView
import web.ui.doc.DocFramer
import web.view.View


abstract class ContentListView[T](
  val onClick: T => Unit,
) extends View with QuickDiff[T] {



  dom = div().render
  private val list = dom

  private var listData: Array[T] = Array.empty.asInstanceOf[Array[T]]

  def size: Int = list.childNodes.length

  protected override def idEq(a: T, b: T): Boolean = a == b

  protected def domAt(i: Int) = list.childNodes(i).asInstanceOf[HTMLElement]

  override protected def performChange(index: Int, oldData: T, newData: T): Unit = {
  }

  def contentOf(t: T): HTMLElement


  protected override def performAdd(i: Int, data: T): Unit = {
    list.insertBefore(contentOf(data), domAt(i))
  }

  protected override def performDelete(i: Int): Unit = {
    web.view.removeFromParent(domAt(i))
  }

  def update(newData0: Array[T]): Unit = {
    val newData = newData0
    diff(listData, newData)
    listData = newData
  }

}
