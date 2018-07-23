package jvm.server

import akka.http.scaladsl.model.{ContentType, ContentTypes, HttpCharset, HttpEntity}
import akka.http.scaladsl.model.StatusCodes.Success
import akka.http.scaladsl.model.{HttpHeader, HttpResponse}
import akka.http.scaladsl.model.headers.`Cache-Control`
import akka.http.scaladsl.model.headers.`Content-Type`
import akka.http.scaladsl.model.headers.CacheDirectives._
import akka.http.scaladsl.server.Directives

import scala.concurrent.ExecutionContext
import akka.http.scaladsl.server.Directives
import akka.http.scaladsl.server.Route
import akka.stream.Materializer
import akka.actor.ActorSystem
import jvm.twirl.Implicits._


import api.Api

class HttpRouter(val service: Api) extends Directives {

  val apiRouter = new AutowireServer(service)

  val route = {

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
