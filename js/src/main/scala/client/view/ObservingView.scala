package client.view

import japgolly.scalajs.react.{Callback, Children}
import japgolly.scalajs.react.component.Scala.BackendScope
import japgolly.scalajs.react.component.builder.Builder
import japgolly.scalajs.react.vdom.VdomElement
import monix.execution.Cancelable
import monix.execution.Scheduler.Implicits.global
import shared.util.ObservableProperty

trait ObservingView[C, T] {
  val $: BackendScope[C, T]
  def render(c: C, t: T): VdomElement
}

object ObservingView {

  def apply[C, T, B <: ObservingView[C, T]](builder: Builder.Step1[C],
    backendBuilder: BackendScope[C, T] => B,
    modeler: C => ObservableProperty[T],
    onStart: C => Unit = (_: C) => Unit,
    onStop: C => Unit = (_: C) => Unit): Builder.Step4[C, Children.None, T, B] = {
    var subscription: Cancelable = null
    builder.initialStateFromProps[T](c => modeler(c).get)
      .backend[B](c => backendBuilder(c))
      .render(a => a.backend.render(a.props, a.state))
      .componentDidMount(data => Callback {
        subscription = modeler(data.props).doOnNext({a =>
          onStart(data.props)
          data.backend.$.setState(a).runNow()
        }).subscribe()
      })
      .componentWillUnmount(a => Callback {
        subscription.cancel()
        onStop(a.props)
      })
  }
}
