package web.view


import client.Client
import command.Key
import scalatags.JsDom.all._
import org.scalajs.dom.raw.{HTMLElement, MouseEvent}

class LeftPanelSwitcher(private val client: Client, enable: Boolean => Unit) extends UnselectableView {


  private val container = div(
    display := "flex",
    marginLeft := "22px",
    width := "calc(100% - 22px)",
    height := "100%"
  ).render

  var widthBefore = "350px"
  val minWidthOpen = "150px"


  private def enabledAll(b: Boolean) = {
    if (b) {
      dom.style.width = widthBefore
      dom.style.minWidth = minWidthOpen
      enable(true)
    } else {
      widthBefore = dom.style.width
      dom.style.minWidth = "22px"
      dom.style.width = "22px"
      enable(false)
    }
  }

  private val commands =
    span(span(`class` := "ct-tab-icon ct-tab-keyboard"), a("Commands")).render

  private val quickAccess = span(span(`class` := "ct-tab-icon ct-tab-quick"), a("Quick Access")).render


  private val childs = Seq(commands, quickAccess)
  for (a <- childs) {
    event(a, "click", (c: MouseEvent) => {
      if (a != active) {
        if (active != null) {
          active.classList.remove("ct-tab-selected")
          current.destroy()
        } else {
          enabledAll(true)
        }
        active = a
        model.localStorage.set(".left-panel", indexOf(active).toString)
        create()
      } else if (active != null) {
        active.classList.remove("ct-tab-selected")
        current.destroy()
        active = null
        model.localStorage.set(".left-panel", "")
        enabledAll(false)
      }
    })
  }


  private var current: View = null

  private def create(): Unit = {
    current = if (active == quickAccess) {
      new TocPanel().attachToNode(container)
    } else if (active == commands) {
      new CommandListPanel(client).attachToNode(container)
    } else {
      null
    }
    if (active != null) {
      active.classList.add("ct-tab-selected")
    }
  }

  private var active = {
    model.localStorage.get(".left-panel") match {
      case Some(a) if a.nonEmpty => childs(a.toInt)
      case _ => null
    }
  }


  dom = div(
    flex := "0 0 auto",
    background := theme.bottomBarBackground,
    minWidth := minWidthOpen,
    width := "350px",
    height := "100%",
    div(position := "absolute", width := "22px", height := "100%", background := "#333842"),
    div(`class` := "ct-tabs",
      childs,
    ),
    container
  ).render

  create()

  if (active == null) {
    enabledAll(false)
  }
}
