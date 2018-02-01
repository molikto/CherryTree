package client.view

import autowire._
import client._
import client.Client
import client.net.AutowireServer
import org.scalajs.dom
import shared.Api
import shared.data._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

class ClientInitializerView(val root: dom.html.Div) {

  private val server = new AutowireServer()[Api]

  {
    // TODO show loading view
    ClientInitializer.init(server, Authentication.Token("")).onComplete {
      case Success(client) =>

      case Failure(exception) =>
        // TODO show retry button
        throw exception
    }
  }
}
