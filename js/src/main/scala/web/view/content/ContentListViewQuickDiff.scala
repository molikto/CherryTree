package web.view.content

import model.NodeQuickDiff
import model.data.{Content, Node}
import org.scalajs.dom.raw.HTMLElement
import web.view.View

class ContentListViewQuickDiff(val list: HTMLElement) extends NodeQuickDiff {

  private var listData: Array[model.data.Node] = Array.empty
  private val headerSize = list.childNodes.length

  private def domAt(i: Int) = list.childNodes(i + headerSize).asInstanceOf[HTMLElement]
  private def viewAt(i: Int) =
    web.view.doc.contentViewFromWithHold(domAt(i))

  override def performChange(index: Int, oldData: Node, newData: Node): Unit = {
    val oldView = viewAt(index)
    if (ContentView.matches(newData.content, oldView)) {
      if (model.debug_view) {
        println(s"performing incremental change $index")
      }
      oldView.updateContent(newData.content)
    } else {
      performDelete(index)
      performAdd(index, newData)
    }
  }

  override def performAdd(i: Int, data: Node): Unit = {
    if (model.debug_view) println(s"performing add $i")
    list.insertBefore(web.view.doc.contentViewAndHold(data), domAt(i))
  }

  override def performDelete(i: Int): Unit = {
    if (model.debug_view) println(s"performing delete $i")
    val oldView = viewAt(i)
    oldView.destroy()
    web.view.removeFromParent(domAt(i))
  }

  def update(newData: Array[model.data.Node]) = {
    diff(listData, newData)
    listData = newData
  }
}
