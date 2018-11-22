package utils.auth

import com.mohiva.play.silhouette.api.actions.SecuredErrorHandler
import javax.inject.Inject
import play.api.i18n.{I18nSupport, Messages, MessagesApi}
import play.api.mvc.RequestHeader
import play.api.mvc.Results._

import scala.concurrent.Future


class CustomSecuredErrorHandler @Inject() (val messagesApi: MessagesApi) extends SecuredErrorHandler with I18nSupport {

  override def onNotAuthenticated(implicit request: RequestHeader) = {
    Future.successful(Redirect(controllers.routes.SignInController.view()))
  }

  def onNotAuthorizedNow(implicit request: RequestHeader) = {
    Redirect(controllers.routes.DocumentsController.documents()).flashing("error" -> Messages("access.denied"))
  }


  override def onNotAuthorized(implicit request: RequestHeader) = {
    Future.successful(onNotAuthorizedNow)
  }
}
