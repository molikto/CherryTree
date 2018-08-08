package web.view

import client.Client
import command.Key
import org.scalajs.dom.raw._
import org.scalajs.dom.window
import scalatags.JsDom.all._
import util.Rect
import web.view.doc.DocumentView

class CommandMenuDialog(val client: Client, protected val layer: OverlayLayer) extends MountedOverlay[OverlayAnchor] {

  private val search = input(
    width := "100%",
    `class` := "ct-input"
  ).render


  override def focus(): Unit = {
    search.focus()
  }

  private val list = div(
    maxWidth := "560px",
    maxHeight := "280px",
    minHeight := "0px",
    overflowY := "scroll",
    overflowX := "hidden",
    color := "#cccccc",
    `class` := "ct-scroll"
  ).render

  dom = div(
    position := "absolute",
    left := "0px",
    top := "0px",
    `class` := "ct-card",
    display := "none",
    div(width := "100%", padding := "6px", search),
    list
  ).render

  private var available: Seq[command.Command] = Seq.empty

  private var marked: command.Command = null
  private var term: String = ""

  observe(client.stateUpdates.doOnNext(u => {
    if (u.mode.isEmpty) {
      dismiss()
    } else {
      // update command list
      updateMenuContent()
    }
  }))

  def updateMenuContent(): Unit = {
    val n = client.commands.filter(a => util.matchCommandSearch(a.description, term) && a.available(client.state, client) && a.keys.isEmpty )
    if (n != available) {
      available = n
      removeAllChild(list)
      if (marked == null) {
        marked = available.head
      }

      for (c <- available.zipWithIndex) {
        list.appendChild(
          div(
            display := "flex",
            flexDirection := "row",
            alignContent := "center",
            `class` := "ct-menu-item " + (if (marked == c._1) "ct-selected" else "ct-not-selected"),
            paddingLeft := "5px",
            if (c._2 < 10)
              span(
                minWidth := "18px",paddingBottom := "2px",
                tag("kbd")(`class` := "ct-kbd-small", c._2),
                " ")
            else span(
              minWidth := "18px"
            ),
            c._1.description
          ).render)
      }
    }
  }

  override def show(anchor: OverlayAnchor): Unit = {
    marked = null
    updateMenuContent()
    search.value = ""
    super.show(anchor)
  }






  override protected def onDismiss(): Unit = {
    super.onDismiss()
    available = Seq.empty
    marked = null
    removeAllChild(list)
    search.value = ""
  }

  private def mark(i: Int): Unit = {
    if (marked != null) {
      val oldIndex = available.indexOf(marked)
      val newIndex = ((oldIndex + i) min (available.size - 1)) max 0
      if (oldIndex != newIndex) {
        marked = available(newIndex)
        val old = list.childNodes(oldIndex).asInstanceOf[HTMLElement]
        val n = list.childNodes(newIndex).asInstanceOf[HTMLElement]
        old.classList.remove("ct-selected")
        old.classList.add("ct-not-selected")
        n.classList.add("ct-selected")
        n.classList.remove("ct-not-selected")
        scrollInToViewIfNotVisible(n, list)
      }
    }
  }

  event(search, "keydown", (ev: KeyboardEvent) => {
    KeyMap.get(ev.key) match {
      case Some(Key.Escape) =>
        ev.preventDefault()
        dismiss()
      case Some(Key.Enter) =>
        ev.preventDefault()
        if (marked != null) {
          client.runTextual(marked)
        }
      case Some(Key.Down) =>
        ev.preventDefault()
        mark(+1)
      case Some(Key.Up) =>
        ev.preventDefault()
        mark(-1)
      case _ =>
        if (term.isEmpty) {
          val nt = ev.key
          if (nt.length == 1 && Character.isDigit(nt.charAt(0))) {
            val n = nt.toInt
            if (n < available.size) {
              ev.preventDefault()
              val command = available(n)
              dismiss()
              client.runTextual(command)
            }
          }
        }
    }
  })

  event(search, "input", (ev: Event) => {
    term = search.value
    marked = null
    updateMenuContent()
  })

  event(dom, "focusout", (ev: FocusEvent) => {
    //dismiss()
  })
}
