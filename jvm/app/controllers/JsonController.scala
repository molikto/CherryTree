package controllers

import akka.actor.ActorSystem
import akka.stream.Materializer
import play.api.libs.json.{JsError, Reads}
import play.api.mvc.{AbstractController, ControllerComponents}

import scala.concurrent.ExecutionContext

class JsonController(components: ControllerComponents)
  (implicit ec: ExecutionContext)
extends AbstractController(components) {


  def validateJson[A : Reads] = parse.json.validate(
    _.validate[A].asEither.left.map(e => BadRequest(JsError.toJson(e)))
  )
}
