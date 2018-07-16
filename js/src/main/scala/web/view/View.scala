package web.view


import monix.execution.Cancelable
import monix.execution.rstreams.Subscription

import scala.collection.mutable.ArrayBuffer
import org.scalajs.dom._
import org.scalajs.dom.raw.HTMLElement

import scala.scalajs.js

object View {

  def fromDom[T](a: Node): T = a.asInstanceOf[js.Dynamic].ctview.asInstanceOf[T]
}

abstract class View {

  private var dom_ : HTMLElement = null

  def dom: HTMLElement = dom_

  def dom_=(a: HTMLElement): Unit = {
    if (dom_ == null) {
      dom_ = a
      dom_.asInstanceOf[js.Dynamic].ctview = this.asInstanceOf[scala.scalajs.js.Any]
    } else {
      throw new IllegalArgumentException("Only set once!!!")
    }
  }

  private val des = ArrayBuffer[Unit => Unit]()

  def destroy(): Unit = {
    des.reverse.foreach(_.apply())
  }

  def defer(a: Unit => Unit): Unit = {
    des.append(a)
  }

  def defer(a: Cancelable): Cancelable = {
    des.append(_ => a.cancel())
    a
  }

  def event[T <: Event](ty: String,
    listener: js.Function1[T, _]): Unit = {
    dom.addEventListener(ty, listener)
    defer(_ => dom.removeEventListener(ty, listener))
  }
}
