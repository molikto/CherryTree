package web.ui.doc

import org.scalajs.dom
import org.scalajs.dom.raw.HTMLElement
import util.Rect
import web.view._
import web.ui.dialog._

abstract class AbstractDocumentView extends View {

  def hasSelection: Boolean

  def selection: org.scalajs.dom.Range


  def startSelection(range: org.scalajs.dom.Range): Unit

  def endSelection(): Unit

  val fakeSelections: HTMLElement


  var sourceEditor: CoveringSourceEditDialog = null
  var commandMenu: CommandMenuDialog = null
  var registersDialog: RegistersDialog = null
  var attributeEditor: UrlAttributeEditDialog = null
  var inlineEditor : InlineCodeDialog = null

  def selectionRect: Rect

  def refreshMounted(): Unit = {
    attributeEditor.refresh()
    inlineEditor.refresh()
    commandMenu.refresh()
    registersDialog.refresh()
  }



  private val commandMenuAnchor = new OverlayAnchor {
    override def rect: Rect = selectionRect
  }

  def showCommandMenu(): Unit = {
    commandMenu.show(commandMenuAnchor)
  }

  def showRegisters(): Unit = {
    registersDialog.show(commandMenuAnchor)
  }
}
