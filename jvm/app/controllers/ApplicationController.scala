package controllers

import java.util.UUID

import com.mohiva.play.silhouette.api.actions.SecuredRequest
import com.mohiva.play.silhouette.api.{LogoutEvent, Silhouette}
import javax.inject.Inject
import play.api.i18n.I18nSupport
import play.api.mvc.{AbstractController, AnyContent, ControllerComponents}
import repos.UserRepository
import utils.auth.DefaultEnv

import scala.concurrent.{ExecutionContext, Future}

class ApplicationController @Inject() (
  components: ControllerComponents,
  silhouette: Silhouette[DefaultEnv],
  users: UserRepository
)(implicit assets: AssetsFinder, ex: ExecutionContext) extends AbstractController(components) with I18nSupport {

  def home = silhouette.SecuredAction.async { implicit  request =>
    Future.successful(Ok(views.html.home(request.identity)))
  }


  def index = silhouette.UserAwareAction.async { implicit request =>
    Future.successful(Ok(views.html.index(request.identity)))
  }

  def default = silhouette.UserAwareAction.async { implicit request =>
    request.identity match {
      case Some(user) =>
        users.indexDocumentId(user.userId).map {
          case Some(documentId) =>
            Redirect(controllers.routes.DocumentController.index(documentId))
          case None =>
            Redirect(controllers.routes.ApplicationController.home())
        }
      case None =>
        Future.successful(Ok(views.html.index()))
    }
  }




  def signOut = silhouette.SecuredAction.async { implicit request =>
    val result = Redirect(routes.ApplicationController.default())
    silhouette.env.eventBus.publish(LogoutEvent(request.identity, request))
    silhouette.env.authenticatorService.discard(request.authenticator, result)
  }
}
