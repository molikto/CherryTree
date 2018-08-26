package web.ui

import client.Client
import org.scalajs.dom._
import org.scalajs.dom.raw._
import web._
import org.scalajs.dom.window
import scalatags.JsDom.all._
import web.ui.dialog._
import web.view.{OverlayLayer, View}
import web.ui.doc.{DocumentView, DocumentView2, EditorView}
import web.ui.panel.LeftPanelSwitcher

import scala.scalajs._

// in this class we use nulls for a various things, but not for public API
class ClientView(private val parent: HTMLElement, val client: Client, val global: Boolean) extends View {

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
    if (web.debug_fakeSelection) {
      div(
        position := "absolute",
        zIndex := "-2",
        `class` := "ct-document-view-background",
        width := "100%",
        height := "100%"
      ) : Frag
    } else Seq.empty[Frag] : Frag,
    overflow := "hidden").render
  attachToNode(parent)

  private var leftPanel: View = null


  private val panelSplitter = div(id := "ctTopPanelSplitter", `class` := "ct-splitter ct-panel", flex := "0 0 auto", width := "4px").render

  leftPanel = new LeftPanelSwitcher(client, () => this, enableResizePanel).attachTo(this)


  dom.appendChild(panelSplitter)

  jQ(leftPanel.dom).resizable(jsObject(a => {
    a.handleSelector = "#ctTopPanelSplitter"
    a.resizeHeight = false
  }))

  override def focus(): Unit = {
    overlayLayer.focus()
  }

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



  private var rootUrl: String = null
  private var duringGoTo = false
  if (global) {
    docView.focus()
    if (rootUrl == null) {
      rootUrl = window.location.href
      if (!rootUrl.endsWith("/")) rootUrl = rootUrl + "/"
    }
    window.history.replaceState(client.state.zoomId, document.title, rootUrl)
    observe(client.stateUpdates.map(_.to.zoomId).distinctUntilChanged.doOnNext(uuid => {
      if (!duringGoTo) {
        // , rootUrl + client.state.nodeRefRelative(uuid)
        window.history.pushState(uuid, document.title)
      }
    }))
    event(window, "popstate", (ev: PopStateEvent) => {
      val uuid = ev.state.asInstanceOf[String]
      duringGoTo = true
      client.state.lookup(uuid).foreach(cur => {
        client.localChange(client.state.goTo(cur, client, mustZoom = true))
      })
      duringGoTo = false
    })
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
    case Client.ViewMessage.CopyToClipboard(str) =>
      util.copyTextToClipboard(str)
    case Client.ViewMessage.SimulateKeyboardMotion(isUp) =>
      docView.simulateKeyboardMotion(isUp)
    case Client.ViewMessage.ScrollToNodeTop(cur) =>
      docView.scrollToTop(cur)
  })


  private val fpsDisplay = div(
    position := "absolute",
    left := "16px",
    top := "16px",
    color := "#FFFFFF",
    padding := "8px",
    backgroundColor := "#FFFFFF22"
  ).render

  if (model.debug_view) {
    dom.appendChild(fpsDisplay)
    _root_.util.debug_fpsView = str => {
      fpsDisplay.textContent = str
    }
  }
}
