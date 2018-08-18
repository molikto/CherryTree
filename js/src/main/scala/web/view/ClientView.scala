package web.view

import client.Client
import org.scalajs.dom._
import org.scalajs.dom.raw._
import web._
import org.scalajs.dom.window
import scalatags.JsDom.all._
import web.view.doc.DocumentView

import scala.scalajs._

// in this class we use nulls for a various things, but not for public API
// TODO stop the client
class ClientView(private val parent: HTMLElement, val client: Client) extends View {

  /**
    *
    *
    * create view
    *
    */

  dom = div(
    width := "100%",
    `class` := "cherrytree",
    height := "100%",
    display :="flex",
    position := "relative",
    flexDirection := "row",
    overflow := "hidden").render
  attachToNode(parent)

  private var leftPanel: View = null


  private val panelSplitter = div(id := "ctTopPanelSplitter", `class` := "ct-splitter ct-panel", flex := "0 0 auto", width := "4px").render

  leftPanel = new LeftPanelSwitcher(client, enableResizePanel).attachTo(this)

  dom.appendChild(panelSplitter)

  jQ(leftPanel.dom).resizable(jsObject(a => {
    a.handleSelector = "#ctTopPanelSplitter"
    a.resizeHeight = false
  }))

  private def enableResizePanel(enable: Boolean): Unit = {
    if (enable) {
      panelSplitter.style.display = "block"
    } else {
      panelSplitter.style.display = "none"
    }
  }

  private val rightPanel = div(
    width := "100%",
    height := "100%",
    display := "flex",
    flexDirection := "column-reverse",
    overflow := "hidden").render
  dom.appendChild(rightPanel)

  new BottomBarView(client).attachToNode(rightPanel)

  private val docView = new DocumentView(client, client).attachToNode(rightPanel).asInstanceOf[DocumentView]

  private val overlayLayer = {
    val o = new OverlayLayer(dom, docView)
    o.attachToNode(dom)
    o
  }

  val quickSearch: QuickSearchDialog = new QuickSearchDialog(client, overlayLayer, dom)

  {
    val commandMenu: CommandMenuDialog = new CommandMenuDialog(client, overlayLayer)
    val sourceEditor: CoveringSourceEditDialog = new CoveringSourceEditDialog(overlayLayer, docView.dom)
    val attributeEditor: UrlAttributeEditDialog = new UrlAttributeEditDialog(overlayLayer)
    val latexEditor: InlineCodeDialog = new InlineCodeDialog(overlayLayer)

    docView.sourceEditor = sourceEditor
    docView.commandMenu = commandMenu
    docView.attributeEditor = attributeEditor
    docView.inlineEditor = latexEditor
  }




  observe(client.viewMessages.doOnNext {
    case Client.ViewMessage.VisitUrl(url) =>
      window.open(url)
    case Client.ViewMessage.ShowCommandMenu() =>
      docView.showCommandMenu()
    case Client.ViewMessage.QuickSearch(viewport) =>
      quickSearch.show(viewport)
    case Client.ViewMessage.ScrollToTop =>
      docView.scrollToTop()
    case Client.ViewMessage.ScrollToBottom =>
      docView.scrollToBottom()
    case Client.ViewMessage.ShowUrlAndTitleAttributeEditor(cur, pos, text) =>
      docView.showAttributeEditor(cur, pos, text)
    case Client.ViewMessage.ShowInlineEditor(cur, pos, init, ty) =>
      docView.showInlineEditor(cur, pos, init, ty)
  })

}
