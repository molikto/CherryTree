package web.ui.dialog

import client.Client
import command.Command
import org.scalajs.dom.raw._
import scalatags.JsDom.all._
import web.view._

class CommandMenuDialog(val client: Client, protected val layer: OverlayLayer) extends  StaticFilteringView[OverlayAnchor, command.Command]  with MountedOverlay[OverlayAnchor] {

  focusOutDismiss = true

  override protected val search = input(
    width := "100%",
    cls := "ct-input"
  ).render

  override protected val list = div(
    width := "100%",
    maxHeight := "280px",
    minHeight := "0px",
    overflowY := "scroll",
    overflowX := "hidden",
    cls := "ct-scroll ct-panel"
  ).render

  dom = div(
    cls := "ct-card",
    div(width := "100%", padding := "6px", search),
    width := "480px",
    list
  ).render



  observe(client.stateUpdates.doOnNext(u => {
    if (!dismissed) {
      if (u.to.mode.isEmpty) {
        dismiss()
      } else {
        // update command list
        update()
      }
    }
  }))

  private val commands = client.commands.commands.filter(_.showInCommandMenu(client.enableModal))

  override def data(term: String): Seq[Command] = {
     commands.filter(a => {
       if (a.textCommand.exists(a => term.startsWith(a) && term.length == a.length + 1 && term.endsWith(" "))) {
         dismiss()
         client.commands.runTextualIfAvailable(a)
         return Seq.empty
       }
      util.matchCommandSearch(a.description, term) &&
        a.available(client.state, client.commands)
    }).sortBy(a => (-a.priority(Seq.empty), a.description))
  }



  override def renderItem(t: Command, index: Int): HTMLElement = {
    div(
      display := "flex",
      flexDirection := "row",
      alignContent := "center",
      cls := "ct-menu-item ",
      t.description
    ).render
  }

  override def onSelected(t: Command): Unit = {
    client.commands.runTextualIfAvailable(t)
  }
}
