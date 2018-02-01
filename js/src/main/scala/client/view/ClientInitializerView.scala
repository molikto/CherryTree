package client.view

import autowire._
import client._
import client.net.JsAutowireAdapter
import org.scalajs.dom
import shared.Api
import shared.client.ClientInitializer
import shared.data._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

class ClientInitializerView(val root: dom.html.Div) {

  private val server = new JsAutowireAdapter()[Api]

  {
    // TODO show loading view
    ClientInitializer.init(server, Authentication.Token("")).onComplete {
      case Success(client) =>
        println("Success")
      case Failure(exception) =>
        // TODO show retry button
        throw exception
    }
  }
}
