package controllers

import java.util.UUID

import com.mohiva.play.silhouette.api.actions.SecuredRequest
import com.mohiva.play.silhouette.api.{LogoutEvent, Silhouette}
import javax.inject.Inject
import play.api.i18n.I18nSupport
import play.api.mvc.{AbstractController, AnyContent, ControllerComponents}
import repos.{DocumentRepository, UserRepository}
import utils.auth.DefaultEnv
import model._
import api._
import jsmessages.JsMessagesFactory

import scala.concurrent.{ExecutionContext, Future}

class ApplicationController @Inject() (
  components: ControllerComponents,
  silhouette: Silhouette[DefaultEnv],
  jsMessagesFactory: JsMessagesFactory,
  users: UserRepository,
  docs: DocumentRepository
)(implicit assets: AssetsFinder, ex: ExecutionContext) extends AbstractController(components) with I18nSupport {


  private val jsMessages = jsMessagesFactory.subset(
    "last.updated",
    "created.at",
    "options",
    "new.from.backup",
    "backup",
    "delete",
    "delete.confirm.title",
      "delete.confirm.message",
    "confirm",
    "cancel",
    "create.new.document"
  )

  val messages = Action { implicit request =>
    Ok(jsMessages(Some("window.Messages")))
  }



  def index = silhouette.UserAwareAction.async { implicit request =>
    Future.successful(Ok(views.html.index(request.identity)))
  }



  def default = silhouette.UserAwareAction.async { implicit request =>
    request.identity match {
      case Some(user) =>
        docs.list(user.userId).map(_.headOption).map {
          case Some(res) =>
            Redirect(controllers.routes.DocumentController.index(res.id))
          case None =>
            Redirect(controllers.routes.DocumentsController.documents())
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
