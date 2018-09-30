package utils.auth

import javax.inject.Inject

import com.mohiva.play.silhouette.api.actions.SecuredErrorHandler
import play.api.i18n.{ MessagesApi, I18nSupport, Messages }
import play.api.mvc.RequestHeader
import play.api.mvc.Results._

import scala.concurrent.Future

class CustomSecuredErrorHandler @Inject() (val messagesApi: MessagesApi) extends SecuredErrorHandler with I18nSupport {

  override def onNotAuthenticated(implicit request: RequestHeader) = {
    Future.successful(Redirect(controllers.routes.SignInController.view()))
  }

  override def onNotAuthorized(implicit request: RequestHeader) = {
    Future.successful(Redirect(controllers.routes.SignInController.view()).flashing("error" -> Messages("access.denied")))
  }
}
