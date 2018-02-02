package client.view

import shared.client.Client
import client.net.JsAutowireAdapter
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.component.Scala.BackendScope
import shared.Api
import shared.client.ClientInitializer
import shared.data._
import japgolly.scalajs.react.vdom.all._
import japgolly.scalajs.react._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}


class ClientInitializerView($: BackendScope[Authentication.Token, Option[Client]]) extends ClientViewDef {

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

  def render(token: Authentication.Token, client: Option[Client]): VdomElement = {
    client match {
      case Some(c) =>
        ClientView(c)
      case None =>
        div(s"$token Connecting to server")
    }
  }
}
