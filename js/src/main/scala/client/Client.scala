package client

import org.scalajs.dom

import scala.scalajs.js
import autowire._
import boopickle.Default._
import client.net.AutowireServer

import scala.util.{Failure, Success}
import scala.concurrent.ExecutionContext.Implicits.global
import org.scalajs.dom
import shared._
import shared.data._

class Client(val root: dom.html.Div) {


  var model: ClientModel = null

  {
    api.authenticate(Authentication.Input("")).call().onComplete {
      case Success(m) =>
        model = new ClientModel(m, null)
      case Failure(f) =>
        println("Failure " + f)
    }
  }
}
