package client.view

import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.component.Scala.BackendScope
import japgolly.scalajs.react.vdom.VdomElement
import shared.client._
import shared.data.ClientState
import shared.client.Client
import client.net.JsAutowireAdapter
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.component.Scala.BackendScope
import shared.Api
import shared.data._
import japgolly.scalajs.react.vdom.all._
import japgolly.scalajs.react._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}


trait ClientViewDef {

  val ClientView = ObservingView[Client, ClientState, ClientView](
    ScalaComponent.builder[Client]("ClientView"),
    s => new ClientView(s),
    client => client.state).build
  //
  //    .initialStateFromProps(c => c.state.get)
  //    .renderBackend[ClientView]
  //    .componentDidMount(_.backend.wire)
  //    .componentWillUnmount(_.backend.unwire)
  //    .build
}

class ClientView(override val $: BackendScope[Client, ClientState]) extends ObservingView[Client, ClientState] {

  def render(client: Client, state: ClientState): VdomElement = {
    div(state.document.root.content)
  }
}
