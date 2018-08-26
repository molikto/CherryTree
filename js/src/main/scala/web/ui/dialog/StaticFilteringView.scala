package web.ui.dialog

import command.Key
import org.scalajs.dom.raw._
import web.view._
import web.ui._

import scala.scalajs.js

trait StaticFilteringView[P <: Any, T] extends OverlayT[P] {


  protected def search: HTMLInputElement

  protected def headerSize: Int = 0
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
      while (list.childNodes.length > headerSize) {
        destroyItem(list.childNodes(headerSize).asInstanceOf[HTMLElement])
      }
      for (c <- available.zipWithIndex) {
        val child = renderItem(c._1, c._2)
        child.addEventListener("click", onClick)
        child.classList.add("ct-flat-selectable")
        if (marked == c._2) child.classList.add( "ct-highlight")
        list.appendChild(child)
      }
    }
  }

  private val onClick: js.Function1[MouseEvent, _] = (e: MouseEvent) => {
    val elm = e.currentTarget.asInstanceOf[HTMLElement]
    marked = indexOf(elm) - headerSize
    if (marked >= 0) {
      val m = available(marked)
      dismiss()
      onSelected(m)
    }
  }

  private def destroyItem(a: HTMLElement): Unit = {
    a.removeEventListener("click", onClick)
    a.parentNode.removeChild(a)
  }

  def data(term: String): Seq[T]

  def renderItem(t: T, index: Int): HTMLElement


  def onSelected(t: T)

  private var available: Seq[T] = Seq.empty


  override def show(t: P): Unit = {
    marked = -1
    search.value = ""
    update()
    super.show(t)
  }

  override protected def onDismiss(): Unit = {
    super.onDismiss()
    available = Seq.empty
    marked = -1
    while (list.childNodes.length > headerSize) {
      val item = list.childNodes(headerSize)
      destroyItem(item.asInstanceOf[HTMLElement])
    }
    term = ""
    search.value = ""
  }

  private def mark(i: Int): Unit = {
    if (marked >= 0) {
      val oldIndex = marked
      val newIndex = ((oldIndex + i) min (available.size - 1)) max 0
      if (oldIndex != newIndex) {
        marked = newIndex
        val old = list.childNodes(oldIndex + headerSize).asInstanceOf[HTMLElement]
        val n = list.childNodes(newIndex + headerSize).asInstanceOf[HTMLElement]
        old.classList.remove("ct-highlight")
        n.classList.add("ct-highlight")
        scrollInToViewIfNotVisible(n, list)
      }
    }
  }

  var focusOutDismiss = false


  def onKeyDown(ev: KeyboardEvent): Boolean = false

  override def onAttach(): Unit = {

    event(search, "input", (ev: Event) => {
      term = search.value
      marked = -1
      update()
    })

    event(dom, "focusout", (ev: FocusEvent) => {
      if (focusOutDismiss) {
        ev.relatedTarget match {
          case h: HTMLElement =>
            if (!dom.contains(h)) {
              dismiss()
            }
          case _ =>
        }
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
          if (!onKeyDown(ev)) {
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
          } else {
            ev.preventDefault()
          }
      }
    })

  }
}
