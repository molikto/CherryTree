package web.view

import client.Client
import command.Key
import org.scalajs.dom.raw._
import org.scalajs.dom.window
import scalatags.JsDom.all._
import util.Rect
import web.view.doc.DocumentView

trait FilteringView[T] extends Overlay {

  protected def search: HTMLInputElement

  protected def list: HTMLElement

  private var term: String = ""

  private var marked: Int = -1

  override def focus(): Unit = {
    search.focus()
  }

  protected def update(): Unit = {
    val oldA = available
    val n = data(term)
    available = n
    if (marked < 0 && available.nonEmpty) {
      marked = 0
    }
    if (n != oldA) {
      while (list.childNodes.length > 0) {
        destroyItem(list.childNodes(0).asInstanceOf[HTMLElement])
      }
      removeAllChild(list)
      for (c <- available.zipWithIndex) {
        val child = renderItem(c._1, c._2)
        child.classList.add(if (marked == c._2) "ct-selected" else "ct-not-selected")
        list.appendChild(child)
      }
    }
  }


  def destroyItem(a: HTMLElement): Unit = {
    a.parentNode.removeChild(a)
  }

  def data(term: String): Seq[T]

  def renderItem(t: T, index: Int): HTMLElement

  def onSelected(t: T)

  private var available: Seq[T] = Seq.empty


  override def show(): Unit = {
    marked = -1
    search.value = ""
    update()
    super.show()
  }

  override protected def onDismiss(): Unit = {
    super.onDismiss()
    available = Seq.empty
    marked = -1
    removeAllChild(list)
    term = ""
    search.value = ""
  }

  private def mark(i: Int): Unit = {
    if (marked >= 0) {
      val oldIndex = marked
      val newIndex = ((oldIndex + i) min (available.size - 1)) max 0
      if (oldIndex != newIndex) {
        marked = newIndex
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

  var focusOutDismiss = false


  override def onAttach(): Unit = {

    event(search, "input", (ev: Event) => {
      term = search.value
      marked = -1
      update()
    })

    event(dom, "focusout", (ev: FocusEvent) => {
      if (focusOutDismiss) {
        dismiss()
      }
    })

    event(search, "keydown", (ev: KeyboardEvent) => {
      KeyMap.get(ev.key) match {
        case Some(Key.Escape) =>
          ev.preventDefault()
          dismiss()
        case Some(Key.Enter) =>
          ev.preventDefault()
          if (marked >= 0) {
            val m = available(marked)
            dismiss()
            onSelected(m)
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
                onSelected(command)
              }
            }
          }
      }
    })

  }
}
