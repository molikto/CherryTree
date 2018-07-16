package web.view

import api.{Api, Authentication}
import client.{Client, ClientInitializer}
import org.scalajs.dom
import web.net.JsAutowireClient

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import scala.util.{Failure, Random, Success}
import scala.concurrent.ExecutionContext.Implicits.global


/**
  * when created, starts connection with server
  * @param where an id of a html element, it is assumed that you don't modify it after handling to us
  */
@JSExportTopLevel("ClientInitializerView")
class ClientInitializerView(where: String) {

  private val rootView = el[dom.html.Element](where)
  private val token = Authentication.Token(System.currentTimeMillis().toString)

  private val server = new JsAutowireClient()[Api]

  var client: Option[Client] = None

  private def goConnecting(): Unit = {
    onlyChild(rootView, {
      import scalatags.JsDom.all._
      p("connecting")
    }.render)
    ClientInitializer.init(server, token).onComplete {
      case Success(c) =>
        goClient(c)
      case Failure(exception) =>
        goFailure(exception)
    }
  }

  goConnecting()


  private def goFailure(exception: Throwable): Unit = {
    onlyChild(rootView, {
      import scalatags.JsDom.all._
      exception.printStackTrace()
      div(
        p(s"failed: ${exception.getMessage}"),
        button("retry", onclick := { () => goConnecting() })
      )
    }.render)
  }


  private def goClient(client: Client): Unit = {
    this.client = Some(client)
    removeAllChild(rootView)
    client.start()
    new ClientView(rootView, client)
  }
}
