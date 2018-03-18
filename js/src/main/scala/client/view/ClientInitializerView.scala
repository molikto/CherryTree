package client.view

import shared.client.ClientModel
import client.net.JsAutowireAdapter
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.component.Scala.BackendScope
import shared.client.ClientInitializer
import shared.data._
import japgolly.scalajs.react.vdom.all._
import japgolly.scalajs.react._
import shared.api.{Api, Authentication}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}


object ClientInitializerView {

  private val creator = ScalaComponent
    .builder[Authentication.Token]("ClientInitializerView")
    .initialState(None.asInstanceOf[Option[ClientModel]])
    .renderBackend[ClientInitializerView]
    .componentDidMount(_.backend.start)
    .componentWillUnmount(_.backend.stop)
    .build
  def apply(token: Authentication.Token) = creator(token)
}

class ClientInitializerView($: BackendScope[Authentication.Token, Option[ClientModel]]) {

  private val server = new JsAutowireAdapter()[Api]

  def stop = Callback {
  }

  def start: Callback = $.props.map { p =>
    ClientInitializer.init(server, p).onComplete {
      case Success(client) =>
        $.setState(Some(client)).runNow()
      case Failure(exception) =>
        exception.printStackTrace()
    }
  }

  def render(token: Authentication.Token, client: Option[ClientModel]): VdomElement = {
    client match {
      case Some(c) =>
        ClientView(c)
      case None =>
        div(s"$token Connecting to server")
    }
  }
}
