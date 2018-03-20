package client.view

import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.component.Scala.{BackendScope, Unmounted}
import shared.client._
import shared.client.ClientModel
import client.net.JsAutowireAdapter
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.component.Scala.BackendScope
import japgolly.scalajs.react.vdom.all._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.Reusability
import shared.api.{Api, Authentication}
import shared.data0.Node
import sun.text.normalizer.ICUBinary.Authenticate

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}


object ClientView {


  private implicit val reusabilityClient = Reusability.always[ClientModel]
  private implicit val reusabilityClientState = Reusability.by_==[Node]

  private val creator = ObserverView[ClientModel, Node, ClientView](
    ScalaComponent.builder[ClientModel]("ClientView"),
    s => new ClientView(s),
    client => client.state,
    onStart = _.start(),
    onStop = _.stop()
  ).configure(Reusability.shouldComponentUpdate)
    .build

  def apply(c: ClientModel): Unmounted[ClientModel, Node, ClientView] = creator(c)
}

class ClientView(override val $: BackendScope[ClientModel, Node]) extends ObserverView[ClientModel, Node] {

  def render(client: ClientModel, state: Node): VdomElement = {
    div(
      div(s"client ${client.debug_authentication}, version ${client.debug_committedVersion}"),
      button("change content", onClick ==> (_ => Callback {
        client.change(Node.Ot.generateRandomTransaction(3, state))
      })),
      div(
        contentEditable := true,
        SimpleTreeView(state)
      )
    )
  }
}
