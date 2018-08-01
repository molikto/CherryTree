package web.view

import client.Client
import org.scalajs.dom._
import org.scalajs.dom.raw._
import org.scalajs.dom.window
import scalatags.JsDom.all._
import web.view.doc.DocumentView

import scala.scalajs._

// in this class we use nulls for a various things, but not for public API
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
    backgroundColor := theme.contentBackground,
    flexDirection := "row",
    overflow := "hidden").render
  attachToNode(parent)

  private val leftPanel = new CommandListView(client).attachTo(this)

  private val panelSplitter = div(id := "ctTopPanelSplitter", `class` := "ct-splitter", flex := "0 0 auto", width := "4px", background := theme.bottomBarBackground).render

  dom.appendChild(panelSplitter)

  jQ(leftPanel.dom).resizable(jsObject(a => {
    a.handleSelector = "#ctTopPanelSplitter"
    a.resizeHeight = false
  }))

  private val rightPanel = div(
    width := "100%",
    height := "100%",
    display := "flex",
    flexDirection := "column-reverse",
    overflow := "hidden").render
  dom.appendChild(rightPanel)

  new BottomBarView(client).attachToNode(rightPanel)

  private val overlayLayer = {
    val o = new OverlayLayer()
    o.attachToNode(dom)
    o
  }

  private val commandMenu: CommandMenuDialog = new CommandMenuDialog(client, _ => docView.focus(), overlayLayer)

  private val docView = new DocumentView(client, client, commandMenu).attachToNode(rightPanel).asInstanceOf[DocumentView]


  observe(client.viewMessages.doOnNext {
    case Client.ViewMessage.VisitUrl(url) =>
      window.open(url)
    case Client.ViewMessage.ShowCommandMenu() =>
      commandMenu.showAt(docView.selectionRect)
    case Client.ViewMessage.ScrollToTop =>
      docView.scrollToTop()
    case Client.ViewMessage.ScrollToBottom =>
      docView.scrollToBottom()
  })

}
