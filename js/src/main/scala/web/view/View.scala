package web.view


import monix.execution.Cancelable
import monix.execution.rstreams.Subscription
import monix.reactive.Observable

import scala.collection.mutable.ArrayBuffer
import org.scalajs.dom._
import org.scalajs.dom.raw.{EventTarget, HTMLElement}

import scala.scalajs.js

object View {

  def fromDom[T <: View](a: Node): T = a.asInstanceOf[js.Dynamic].ctview.asInstanceOf[T]

  def maybeDom[T <: View](a: Node): Option[T] = {
    val tt = a.asInstanceOf[js.Dynamic].ctview
    if (tt != js.undefined && tt != null && tt.isInstanceOf[T]) {
      Some(tt.asInstanceOf[T])
    } else {
      None
    }
  }

  private val views = new ArrayBuffer[View]()

  if (model.debug_view) {
    window.setInterval(() => {
      for (v <- views) {
        if (v.dom_ != null && v.attached && !document.body.contains(v.dom_) && !v.destroyed) {
          window.console.log(v.dom)
          throw new IllegalStateException("View detached but not destroyed")
        }
      }
    }, 3000)
  }
}

abstract class View {

  private var dom_ : HTMLElement = null
  private var attached = false
  private var des = ArrayBuffer[Unit => Unit]()

  if (model.debug_view) {
    View.views.append(this)
  }

  def dom: HTMLElement = {
    if (destroyed) throw new IllegalArgumentException("Already destroyed")
    dom_
  }

  def dom_=(a: HTMLElement): Unit = {
    if (dom_ == null) {
      dom_ = a
      dom_.asInstanceOf[js.Dynamic].ctview = this.asInstanceOf[scala.scalajs.js.Any]
    } else {
      throw new IllegalArgumentException("Only set once!!!")
    }
  }


  def attachToNode(a: Node, before: HTMLElement = null) : View = {
    if (destroyed) throw new IllegalStateException("Not supported")
    attached = true
    a.insertBefore(dom, before)
    onAttach()
    this
  }
  def attachTo(a: View, before: HTMLElement = null): View = {
    if (destroyed) throw new IllegalStateException("Not supported")
    attached = true
    a.dom.insertBefore(dom, before)
    onAttach()
    this
  }

  def onAttach(): Unit = {

  }

  def destroyed: Boolean = des == null

  /**
    * will also remove from parent
    * ALSO make sure you destroy child dom attachments!!!
    */
  def destroy(): Unit = {
    des.reverse.foreach(_.apply())
    dom_.parentNode.removeChild(dom_)
    des = null
  }

  def defer(a: Unit => Unit): Unit = {
    des.append(a)
  }

  /**
    *
    * the cancelable returned is THE SAME as the argument, and you are not being able to cancel a defer!!!
    * // LATER cancel this
    */
  def observe[T](a: Observable[T]): Cancelable = {
    import monix.execution.Scheduler.Implicits.global
    val cancelable = a.subscribe()
    des.append(_ => cancelable.cancel())
    cancelable
  }

  def event[T <: Event](ty: String,
    listener: js.Function1[T, _]): Unit = {
    event(dom, ty, listener)
  }

  def event[T <: Event](node: EventTarget, ty: String,
    listener: js.Function1[T, _]): Unit = {
    if (des == null) throw new IllegalAccessException("Destroyed!")
    node.addEventListener(ty, listener)
    defer(_ => node.removeEventListener(ty, listener))
  }

  def focus(): Unit = {
    dom.focus()
  }

  def scrollToTop(): Unit =
    dom.scrollTop = 0

  def scrollToBottom(): Unit =
  dom.scrollTop = dom.scrollHeight - dom.clientHeight

  def preventDefault(a: Event,notCancelable: () => Unit = () => console.log("Not cancelable event not handled")): Unit  = {
    if (a.cancelable) {
      a.preventDefault()
    } else {
      notCancelable()
    }
  }
}
