package jvm.server

import akka.http.scaladsl.model._
import akka.http.scaladsl.model.StatusCodes.Success
import akka.http.scaladsl.model.headers.`Cache-Control`
import akka.http.scaladsl.model.headers.`Content-Type`
import akka.http.scaladsl.model.headers.CacheDirectives._
import akka.http.scaladsl.server.Directives

import scala.concurrent.ExecutionContext
import akka.http.scaladsl.server.Directives
import akka.http.scaladsl.server.Route
import akka.stream.Materializer
import akka.actor.{Actor, ActorSystem}
import akka.http.scaladsl.model.ws._
import akka.stream.scaladsl.Flow
import akka.stream.stage.GraphStage
import jvm.views.Implicits._
import api.Api

class HttpRouter(val service: Api) extends Directives {

  val apiRouter = new AutowireServer(service)

  class DocumentActor extends Actor {
    override def receive: Receive = ???
  }

  def apply()(implicit s: ActorSystem, m: Materializer, e: ExecutionContext): Route = {
    redirectToNoTrailingSlashIfPresent(StatusCodes.Found) {
      pathEnd {
        get {
          cookie("token") { token => // if user logged in, redirect to last edited document
            redirect("document/", StatusCodes.Found)
          } ~ {
            complete {
              twirl.html.main.render()
            }
          }
        }
      } ~ pathPrefix("document") {
        pathEnd {
          complete {
            twirl.html.main.render()
          }
        } ~ path(Remaining) { documentId =>
          handleWebSocketMessages()
          extractUpgradeToWebSocket { it =>
            complete {
              it.handleMessagesWithSinkSource(Graph)
            }
          }
        }
        // bundled client
      } ~ (pathPrefix("assets" / Remaining) & respondWithHeader(`Cache-Control`(`no-cache`))) { file =>
        // optionally compresses the response with Gzip or Deflate
        // if the client accepts compressed responses
        getFromResource("public/" + file)
      } ~
        path("api" / Segments) { segments =>
          post(apiRouter.dispatch(segments))
        }
    }

    pathSingleSlash {
      get {
        complete {
          twirl.html.main.render()
        }
      }
    } ~
    (pathPrefix("assets" / Remaining) & respondWithHeader(`Cache-Control`(`no-cache`))) { file =>
      // optionally compresses the response with Gzip or Deflate
      // if the client accepts compressed responses
      getFromResource("public/" + file)
    } ~
    path("api" / Segments) { segments =>
      post(apiRouter.dispatch(segments))
    }
  }
}
