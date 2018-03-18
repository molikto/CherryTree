package client.view

import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.component.Scala.{BackendScope, Unmounted}
import shared.client._
import shared.data.ClientState
import shared.client.ClientModel
import client.net.JsAutowireAdapter
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.component.Scala.BackendScope
import shared.data._
import japgolly.scalajs.react.vdom.all._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.Reusability
import shared.api.Api

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}


object ClientView {


  private implicit val reusabilityClient = Reusability.always[ClientModel]
  private implicit val reusabilityClientState = Reusability.by_==[ClientState]

  private val creator = ObserverView[ClientModel, ClientState, ClientView](
    ScalaComponent.builder[ClientModel]("ClientView"),
    s => new ClientView(s),
    client => client.state,
    onStart = _.start(),
    onStop = _.stop()
  ).configure(Reusability.shouldComponentUpdate)
    .build

  def apply(c: ClientModel): Unmounted[ClientModel, ClientState, ClientView] = creator(c)
}

class ClientView(override val $: BackendScope[ClientModel, ClientState]) extends ObserverView[ClientModel, ClientState] {

  def render(client: ClientModel, state: ClientState): VdomElement = {
    div(
      div(s"client ${state.authentication}, version ${state.document.version}"),
      button("change content", onClick ==> (_ => Callback {
        client.change(shared.test.randomTwoChangeTransaction(client.state.document.root))
      })),
      div(
        contentEditable := true,
        SimpleTreeView(state.document.root)
      )
    )
  }
}
