package web.view

import client.Client
import org.scalajs.dom.raw._
import org.scalajs.dom.window
import scalatags.JsDom.all._
import web.view.doc.DocumentView

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
    backgroundColor := theme.contentBackground,
    flexDirection := "column-reverse",
    overflow := "hidden").render
  parent.appendChild(dom)

  new BottomBarView(dom, client)

  private val topPanels = div(
    width := "100%",
    height := "100%",
    display := "flex",
    flexDirection := "row",
    overflow := "hidden").render
  dom.appendChild(topPanels)

  val leftPanel = new CommandListView(topPanels, client)

  private val topPanelSplitter = div(id := "ctTopPanelSplitter", `class` := "ct-splitter", flex := "0 0 auto", width := "4px", background := theme.bottomBarBackground).render
  topPanels.appendChild(topPanelSplitter)


  new DocumentView(topPanels, client, client)


  jQ(leftPanel.dom).resizable(jsObject(a => {
    a.handleSelector = "#ctTopPanelSplitter"
    a.resizeHeight = "false"
  }))


  observe(client.viewMessages.doOnNext {
    case Client.ViewMessage.VisitUrl(url) =>
      window.open(url)
  })

}
