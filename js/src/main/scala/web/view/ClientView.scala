package web.view

import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.component.Scala.{BackendScope, Unmounted}
import controller.client._
import controller.client.ClientModel
import controller.net.JsAutowireAdapter
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.component.Scala.BackendScope
import japgolly.scalajs.react.vdom.all._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.Reusability
import controller.api.{Api, Authentication}
import sun.text.normalizer.ICUBinary.Authenticate
import shared._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}


object ClientView {


  private implicit val reusabilityClient = Reusability.always[ClientModel]
  private implicit val reusabilityClientState = Reusability.by_==[model.Node]

  private val creator = ObserverView[ClientModel, model.Node, ClientView](
    ScalaComponent.builder[ClientModel]("ClientView"),
    s => new ClientView(s),
    client => client.doc,
    onStart = _.start(),
    onStop = _.stop()
  ).configure(Reusability.shouldComponentUpdate)
    .build

  def apply(c: ClientModel): Unmounted[ClientModel, model.Node, ClientView] = creator(c)
}

class ClientView(override val $: BackendScope[ClientModel, model.Node]) extends ObserverView[ClientModel, model.Node] {

  def render(client: ClientModel, state: model.Node): VdomElement = {
    div(
      div(s"client ${client.debug_authentication}, version ${client.debug_committedVersion}"),
      button("change content", onClick ==> (_ => Callback {
        client.change(ot.node.generateRandomTransaction(3, state))
      })),
      div(
        contentEditable := true,
        SimpleTreeView(state)
      )
    )
  }
}
