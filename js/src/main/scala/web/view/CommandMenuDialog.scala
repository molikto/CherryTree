package web.view

import client.Client
import command.Key
import org.scalajs.dom.raw._
import org.scalajs.dom.window
import scalatags.JsDom.all._
import util.Rect
import web.view.doc.DocumentView

class CommandMenuDialog(val client: Client, protected val layer: OverlayLayer) extends Overlay {

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
    overflowY := "scroll",
    paddingRight := "-10px",
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
    if (available.nonEmpty) {
      // update command list
      updateMenuContent()
    }
  }))

  def updateMenuContent(): Unit = {
    val n = client.commands.filter(a => a.available(client.state, client) && a.keys.isEmpty )
    if (n != available) {
      available = n
      if (available.isEmpty) {
        dismiss()
      } else {
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
  }

  def whereToShow(bounding: Rect, rec: Rect) = {

  }

  def showAt(rect: Rect): Unit = {
    val bounding = toRect(layer.dom.getBoundingClientRect())
    val rec = rect.moveBy(-bounding.left, -bounding.top)
    //whereToShow(bounding, rec)
    dom.style.left = rec.left.toString + "px"
    dom.style.top = rec.bottom.toString + "px"
    updateMenuContent()
    search.textContent = ""
    showOverlay()
  }





  override protected def onDismiss(): Unit = {
    super.onDismiss()
    available = Seq.empty
    marked = null
    search.textContent = ""
  }

  private def mark(i: Int): Unit = {
    val oldIndex = available.indexOf(marked)
    val newIndex = ((oldIndex + i) min available.size - 1) max 0
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
    term = search.textContent
    updateMenuContent()
  })

  event(dom, "focusout", (ev: FocusEvent) => {
    dismiss()
  })
}
