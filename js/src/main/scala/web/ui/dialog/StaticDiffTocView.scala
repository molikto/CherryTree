package web.ui.dialog

import model.data.Node
import org.scalajs.dom.raw.HTMLElement

class StaticDiffTocView(onClick: String => Unit) extends StaticDiffContentListView(onClick) {

  protected override def eq(a: Node, b: Node): Boolean = a == b

  protected override def performChange(index: Int, oldData: Node, newData: Node): Unit = {
    if (!super.eq(oldData, newData)) {
      super.performChange(index, oldData, newData)
    }
    web.view.View.fromDom[StaticDiffTocView](extraViewInFrame(domAt(index))).update(newData.childs)
  }

  override protected def onAddViewAndHold(view: HTMLElement, data: Node): Unit = {
    val od = new StaticDiffTocView(onClick)
    od.update(data.childs)
    insertExtraToContentView(view, od.dom)
  }

  override def update(newData0: Seq[Node]): Unit = {
    super.update(newData0.filter(a => a.isHeading && !a.isH1))
  }

  override def updateFocus(uuid: Option[String], list: HTMLElement): Unit = {
    super.updateFocus(uuid, list)
    (0 until size).foreach(i => {
      web.view.View.fromDom[StaticDiffTocView](extraViewInFrame(domAt(i))).updateFocus(uuid, list)
    })
  }
}
