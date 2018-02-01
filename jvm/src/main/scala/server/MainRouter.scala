package server
import shared.{Api, Ids}
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

class MainRouter(val service: Api) extends Directives {

  val apiRouter = new ApiRouter(service)

  def apply()(implicit s: ActorSystem, m: Materializer, e: ExecutionContext): Route = {

    pathSingleSlash {
      get {
        complete {
          HttpEntity(ContentTypes.`text/html(UTF-8)`, string =
            s"""|<!DOCTYPE html>
                |<html>
                |  <head>
                |    <title>Cherry Tree Dev Test</title>
                |    <link href="https://cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/4.0.0/css/bootstrap-reboot.min.css" rel="stylesheet" type="text/css" />
                |  </head>
                |  <body>
                |   <div id="${Ids.main}"></div>
                |   <img src="/assets/images/test.png" />
                |   <script src="/assets/client-fastopt-bundle.js" type="text/javascript"></script>
                |  </body>
                |</html>""".stripMargin)
        }
      }
    } ~
    ((pathPrefix("assets" / Remaining) & respondWithHeader(`Cache-Control`(`no-cache`)))) { file =>
      // optionally compresses the response with Gzip or Deflate
      // if the client accepts compressed responses
      getFromResource("public/" + file)
    } ~
    path("api" / Segments) { segments =>
      post(apiRouter.dispatch(segments))
    }
  }
}
