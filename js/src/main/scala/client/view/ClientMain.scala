package client.view

import autowire._
import client._
import client.Client
import org.scalajs.dom
import shared.data._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

class ClientMain(val root: dom.html.Div) {

  {
    ClientInitializer.init(Authentication.Token("")).onComplete {
      case Success(client) =>
        root.textContent = "created client"
      case Failure(exception) =>
        throw exception
    }
  }
}
