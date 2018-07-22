package web.view


import monix.execution.Cancelable
import monix.execution.rstreams.Subscription
import monix.reactive.Observable

import scala.collection.mutable.ArrayBuffer
import org.scalajs.dom._
import org.scalajs.dom.raw.HTMLElement

import scala.scalajs.js

object View {

  def fromDom[T <: View](a: Node): T = a.asInstanceOf[js.Dynamic].ctview.asInstanceOf[T]
}

abstract class View {

  private var dom_ : HTMLElement = null

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

  private var des = ArrayBuffer[Unit => Unit]()


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
    if (des == null) throw new IllegalAccessException("Destroyed!")
    dom.addEventListener(ty, listener)
    defer(_ => dom.removeEventListener(ty, listener))
  }

  def event[T <: Event](node: HTMLElement, ty: String,
    listener: js.Function1[T, _]): Unit = {
    if (des == null) throw new IllegalAccessException("Destroyed!")
    node.addEventListener(ty, listener)
    defer(_ => node.removeEventListener(ty, listener))
  }

  def cancel(a: Event,notCancelable: () => Unit = () => console.log("Not cancelable event not handled")): Unit  = {
    if (a.cancelable) {
      a.preventDefault()
    } else {
      notCancelable
    }
  }
}
