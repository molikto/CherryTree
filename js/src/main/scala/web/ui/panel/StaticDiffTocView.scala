package web.ui.panel

import java.util.UUID

import model.data.Node
import org.scalajs.dom.raw.HTMLElement
import scalatags.JsDom.all._
import web.ui.doc.LaTeXMacroCache

class StaticDiffTocView(
  onClick: UUID => Unit,
  onDoubleClick: UUID => Unit,
  override val latexMacroCache: LaTeXMacroCache,
  override val parentHeadingLevel: Int) extends StaticDiffContentListView(onClick, onDoubleClick, latexMacroCache) {

  protected override def eq(a: Node, b: Node): Boolean = a == b

  protected override def performChange(index: Int, oldData: Node, newData: Node): Unit = {
    if (!super.eq(oldData, newData)) {
      super.performChange(index, oldData, newData)
    }
    web.view.View.fromDom[StaticDiffTocView](extraViewInFrame(domAt(index))).update(newData.childs)
  }
  
//  val header = scalatags.JsDom.all.div(s"${parentHeadingLevel}" : Frag).render
//
//  addHeader(header)

  override protected def onAddViewAndHold(view: HTMLElement, data: Node): Unit = {
    val od = new StaticDiffTocView(onClick, onDoubleClick, latexMacroCache, parentHeadingLevel + 1)
    od.update(data.childs)
    od.dom.classList.add("ct-d-margin-left")
    insertExtraToContentView(view, od.dom)
  }

  override def update(newData0: Seq[Node]): Unit = {
    super.update(newData0.filter(a => a.isHeading && !a.isFolder))
  }

  def updateFocus(uuid: Option[UUID], hideLevel: Int, list: HTMLElement): Boolean = {
    var containsId = super.updateFocus(uuid, list)
    var contains = containsId.isDefined
    (0 until size).foreach(i => {
      contains = web.view.View.fromDom[StaticDiffTocView](extraViewInFrame(domAt(i))).updateFocus(uuid, hideLevel, list) || contains // order matters
    })
    containsId.foreach(i => {
      extraViewInFrame(domAt(i)).style.display = "block"
    })
    dom.style.display = if (!contains && parentHeadingLevel + 1 >= hideLevel) "none" else "block"
//    dom.style.opacity = if (!contains && parentHeadingLevel + 1 >= hideLevel) "0.5" else "1"
    //header.textContent = s"$parentHeadingLevel $hideLevel $contains"
    contains
  }
}
