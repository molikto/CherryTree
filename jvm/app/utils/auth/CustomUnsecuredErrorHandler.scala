package utils.auth

import com.mohiva.play.silhouette.api.actions.UnsecuredErrorHandler
import play.api.mvc.RequestHeader
import play.api.mvc.Results._

import scala.concurrent.Future

class CustomUnsecuredErrorHandler extends UnsecuredErrorHandler {

  override def onNotAuthorized(implicit request: RequestHeader) = {
    Future.successful(Redirect(controllers.routes.ApplicationController.index()))
  }
}
