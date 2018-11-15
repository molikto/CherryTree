package web.ui.panel

import client.Client
import client.Client.ViewMessage.QuickSearch
import org.scalajs.dom.raw.MouseEvent
import scalatags.JsDom.all.{a, _}
import web.WebApi
import web.ui.dialog.{QuickSearchDialog, SettingsDialog}
import web.ui.doc.{DocumentView, LaTeXMacroCache}
import web.view.{UnselectableView, View, indexOf}

class LeftPanelSwitcher(private val cl: Client, 
  doc: => View,
  settingsDialog : => SettingsDialog,
  quickSearch: => QuickSearchDialog,
  laTeXMacroCache: LaTeXMacroCache,
  enable: Boolean => Unit) extends UnselectableView {


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
    span(
      cls := "ct-tab-item",
      span(cls := "ct-tab-icon ct-tab-keyboard"), a("Commands")).render

  private val quickAccess = span(
    cls := "ct-tab-item",
    span(cls := "ct-tab-icon ct-tab-quick"), a("Quick Access")).render

  private val undoHistory = span(
    cls := "ct-tab-item",
    span(cls := "ct-tab-icon ct-tab-bug"), a("Undos")).render

  private val tags = span(
    cls := "ct-tab-item",
    span(cls := "ct-tab-icon ct-tab-tags"), a("Tags")).render

  private val settings = span(
    cls := "ct-tab-item",
    span(cls := "ct-tab-icon ct-tab-settings"), a("Settings")).render

  private var childs = Seq(settings, commands, tags, quickAccess)

  if (model.debug_view) {
    childs = undoHistory +: childs
  }
  for (a <- childs) {
    if (a == settings) {
      event(settings, "click", (c: MouseEvent) => {
        settingsDialog.show(Unit)
      })
    } else {
      event(a, "click", (c: MouseEvent) => {
        if (a != active) {
          if (active != null) {
            active.classList.remove("ct-tab-selected")
            current.destroy()
          } else {
            enabledAll(true)
          }
          active = a
          WebApi.localStorage.set(".left-panel", indexOf(active).toString)
          create()
        } else if (active != null) {
          active.classList.remove("ct-tab-selected")
          current.destroy()
          current = null
          active = null
          WebApi.localStorage.set(".left-panel", "")
          enabledAll(false)
        }
        doc.focus()
      })
    }
  }


  private var current: View = null

  private def create(): Unit = {
    current = if (active == quickAccess) {
      new QuickAccessPanel(cl, doc, laTeXMacroCache).attachToNode(container)
    } else if (active == commands) {
      new CommandListPanel(cl, doc).attachToNode(container)
    } else if (active == tags) {
      new TagsPanel(cl, doc, quickSearch, laTeXMacroCache).attachToNode(container)
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
    WebApi.localStorage.get(".left-panel") match {
      case Some(a) if a.nonEmpty && a.toInt < childs.size => childs(a.toInt)
      case _ => null
    }
  }


  dom = div(
    flex := "0 0 auto",
    cls := "ct-panel",
    minWidth := minWidthOpen,
    width := "350px",
    height := "100%",
    div(position := "absolute", width := "22px", height := "100%", background := "#333842"),
    p(cls := "ct-tabs",
      marginBottom := "0px",
      childs,
    ),
    container
  ).render

  create()

  if (active == null) {
    enabledAll(false)
  }

  /**
    * will also remove from parent
    * ALSO make sure you destroy child dom attachments!!!
    */
  override def destroy(): Unit = {
    if (current != null) current.destroy()
    current = null
    super.destroy()
  }
}
