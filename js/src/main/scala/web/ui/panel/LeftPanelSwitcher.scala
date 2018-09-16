package web.ui.panel

import client.Client
import org.scalajs.dom.raw.MouseEvent
import scalatags.JsDom.all.{a, _}
import web.view.{UnselectableView, View, indexOf}

class LeftPanelSwitcher(private val cl: Client, doc: () => View, enable: Boolean => Unit) extends UnselectableView {


  private val container = div(
    display := "flex",
    marginLeft := "22px",
    minWidth := "240px",
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

  private val undoHistory = span(span(`class` := "ct-tab-icon ct-tab-bug"), a("Undo History")).render

  private val tags = span(span(`class` := "ct-tab-icon ct-tab-tags"), a("Tags")).render

  private var childs = Seq(tags, commands, quickAccess)

  if (model.debug_view) {
    childs = undoHistory +: childs
  }
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
        client.localStorage.set(".left-panel", indexOf(active).toString)
        create()
      } else if (active != null) {
        active.classList.remove("ct-tab-selected")
        current.destroy()
        active = null
        client.localStorage.set(".left-panel", "")
        enabledAll(false)
      }
      doc().focus()
    })
  }


  private var current: View = null

  private def create(): Unit = {
    current = if (active == quickAccess) {
      new QuickAccessPanel(cl, doc).attachToNode(container)
    } else if (active == commands) {
      new CommandListPanel(cl, doc).attachToNode(container)
    } else if (active == tags) {
      new TagsPanel(cl, doc).attachToNode(container)
    } else if (active == undoHistory) {
      new UndoHistoryPanel(cl).attachToNode(container)
    } else {
      null
    }
    if (active != null) {
      active.classList.add("ct-tab-selected")
    }
  }

  private var active = {
    client.localStorage.get(".left-panel") match {
      case Some(a) if a.nonEmpty && a.toInt < childs.size => childs(a.toInt)
      case _ => null
    }
  }


  dom = div(
    flex := "0 0 auto",
    `class` := "ct-panel",
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
