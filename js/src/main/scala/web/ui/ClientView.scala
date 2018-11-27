package web.ui

import java.util.UUID

import api.PermissionLevel
import client.Client
import io.lemonlabs.uri.Url
import org.scalajs.dom._
import org.scalajs.dom.raw._
import web._
import org.scalajs.dom.window
import scalatags.JsDom.all._
import web.ui.dialog._
import web.view.{OverlayLayer, View}
import web.ui.doc.{LaTeXMacroCache, SimpleLayoutDocumentView}
import web.ui.panel.LeftPanelSwitcher

import scala.scalajs._

// in this class we use nulls for a various things, but not for public API
class ClientView(private val parent: HTMLElement, val client: Client, val onSettingChangeRefresh: () => Unit, val global: Boolean) extends View {

  /**
    *
    *
    * create view
    *
    */

  val latexMacroCache = new LaTeXMacroCache()

  dom = div(
    width := "100%",
    cls := "cherrytree",
    height := "100%",
    display :="flex",
    position := "relative",
    flexDirection := "row",
    overflow := "hidden").render
  attachToNode(parent)

  private var leftPanel: LeftPanelSwitcher = null


  private val panelSplitter = div(id := "ctTopPanelSplitter", cls := "ct-splitter ct-panel", flex := "0 0 auto", width := "4px").render

  leftPanel = defer(new LeftPanelSwitcher(client, this, settingsDialog, quickSearch, latexMacroCache, enableResizePanel))

  {
    leftPanel.attachTo(this)
  }


  dom.appendChild(panelSplitter)

  jQ(leftPanel.dom).resizable(jsObject(a => {
    a.handleSelector = "#ctTopPanelSplitter"
    a.resizeHeight = false
  }))


  override def onAttach(): Unit = {
    super.onAttach()
    client.preStateUpdate = Some(update => {
      latexMacroCache.update(update.to)
      if (latexMacroCache.dirty) {
        latexMacroCache.rebuildAndMarkNoDirty()
        docView.postRefreshAllLaTeX()
      }
    })
    latexMacroCache.update(client.state)
    latexMacroCache.rebuildAndMarkNoDirty()
  }

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
    minWidth := "360px",
    position := "relative",
    flexDirection := "column-reverse",
    overflow := "hidden").render
  dom.appendChild(rightPanel)

  val bottomBar = defer(new BottomBarView(client))
  bottomBar.attachToNode(rightPanel)


  private val docView = defer(new SimpleLayoutDocumentView(client, client, client, latexMacroCache).attachToNode(rightPanel).asInstanceOf[SimpleLayoutDocumentView])

  private val overlayLayer = {
    val o = defer(new OverlayLayer(dom, docView))
    o.attachToNode(dom)
    o
  }

  lazy val settingsDialog: SettingsDialog = defer(new SettingsDialog(client, overlayLayer, onSettingChangeRefresh, dom))

  lazy val quickSearch: QuickSearchDialog = defer(new QuickSearchDialog(client, overlayLayer, dom, latexMacroCache))


  private val searchBar = defer(new SearchBar(client.searchHandler, () => docView, bottomBar.size).attachToNode(rightPanel))

  val commandMenu: CommandMenuDialog = defer(new CommandMenuDialog(client, overlayLayer))
  val registers: RegistersDialog = defer(new RegistersDialog(client, overlayLayer, latexMacroCache))
  val sourceEditor: CoveringSourceEditDialog = defer(new CoveringSourceEditDialog(client, client, overlayLayer, docView.dom))
  val attributeEditor: AttributeEditDialog = defer(new AttributeEditDialog(overlayLayer))
  val latexEditor: InlineCodeDialog = defer(new InlineCodeDialog(client, client, overlayLayer))

  {
    docView.sourceEditor = sourceEditor
    docView.commandMenu = commandMenu
    docView.registersDialog = registers
    docView.attributeEditor = attributeEditor
    docView.inlineEditor = latexEditor
  }



  private var rootUrl: String = null
  private var duringGoTo = false


  if (global) {
    docView.focus()
    if (rootUrl == null) {
      rootUrl = Url.parse(window.location.href).removeQueryString().toString()
    }
    def formatUrl(uuid: UUID, zoomToEmpty: Boolean = true) = {
      rootUrl + client.state.nodeRefRelative(uuid, zoomToEmpty)
    }
    if (window.location.href != rootUrl && window.location.href != formatUrl(client.state.zoomId, false)) {
      window.history.pushState(client.state.zoomId.toString, document.title, rootUrl)
      window.setTimeout(() => {
        alert("Your url reference to a non-existing node", "warning")
      }, 0.0)
    }
    observe(client.stateUpdates.map(_.to.zoomId).distinctUntilChanged.doOnNext(uuid => {
      if (!duringGoTo) {
        window.history.pushState(uuid.toString, document.title, formatUrl(uuid))
      }
    }))
    event(window, "popstate", (ev: PopStateEvent) => {
      val uuid = UUID.fromString(ev.state.asInstanceOf[String])
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
    case Client.ViewMessage.ShowRegisters() =>
      docView.showRegisters()
    case Client.ViewMessage.QuickSearch(viewport) =>
      quickSearch.show(viewport)
    case Client.ViewMessage.ScrollToTop =>
      docView.scrollToTop()
    case Client.ViewMessage.ScrollToBottom =>
      docView.scrollToBottom()
    case Client.ViewMessage.CopyToClipboard(str) =>
      util.copyTextToClipboard(str)
    case Client.ViewMessage.VisualUpDownMotion(isUp, blockWiseCount, enterVisual) =>
      docView.visualUpDownMotion(isUp, blockWiseCount, enterVisual)
    case Client.ViewMessage.ExitVisual =>
      docView.exitVisual()
    case Client.ViewMessage.ScrollToNodeTop(cur) =>
      docView.scrollToTop(cur)
  })


  val alerts = div(
    position := "absolute",
    cls := "unselectable",
    width := "100%",
    overflow := "none",
    bottom := "48px",
    left := "12px",
    right := "12px"
  ).render

  def alert(msg: String, typ: String) = alerts.appendChild(div(cls := s"alert alert-${typ} alert-dismissible fade show", role := "alert",
    msg,
    button(`type` := "button", cls := "close", attr("data-dismiss") := "alert", attr("aria-label") := "Close",
      span(attr("aria-hidden") := "true","×")
    )
  ).render)

  rightPanel.appendChild(alerts)



  if (client.permissionLevel >= PermissionLevel.Edit) {
  } else if (client.permissionLevel >= PermissionLevel.Comment) {
    alert("You only have comment permission.", "warning")
  } else {
    alert("You only have read permission", "warning")
  }


  private val fpsDisplay = div(
    position := "absolute",
    left := "16px",
    bottom := "16px",
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
