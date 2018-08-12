package web.view


import client.Client
import command.Key
import scalatags.JsDom.all._
import org.scalajs.dom.raw.{HTMLElement, MouseEvent}

class LeftPanelSwitcher(private val client: Client) extends UnselectableView {


  private val container = div(
    display :="flex",
    marginLeft := "22px",
    width := "calc(100% - 22px)",
    height := "100%"
  ).render

  object tab {
    val commands =
      span(span(`class` := "ct-tab-icon ct-tab-keyboard"), a("Commands")).render

    val quickAccess = span(`class` := "ct-tab-selected", span(`class` := "ct-tab-icon ct-tab-quick"), a("Quick Access")).render

    for (a <- Seq(commands, quickAccess)) {
      event(a, "click", (c: MouseEvent) => {
        if (a != active) {
          a.classList.add("ct-tab-selected")
          active.classList.remove("ct-tab-selected")
          current.destroy()
          active = a
          create()
        }
      })
    }
    private var active = quickAccess

    private var current: View = null

    def create(): Unit = {
      current = if (active == quickAccess) {
        new QuickAccessView().attachToNode(container)
      } else {
        new CommandListView(client).attachToNode(container)
      }
    }
  }


  dom = div(
    flex := "0 0 auto",
    background := theme.bottomBarBackground,
    minWidth := "150px",
    width := "350px",
    height := "100%",
    div(position := "absolute", width := "22px", height := "100%", background := "#333842"),
    div( `class` := "ct-tabs",
      tab.commands,
      tab.quickAccess
    ),
    container
  ).render

  tab.create()
}
