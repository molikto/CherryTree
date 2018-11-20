package controllers

import java.io.FileInputStream
import java.nio.ByteBuffer
import java.nio.file.Files
import java.util.UUID

import com.mohiva.play.silhouette.api.actions.SecuredRequest
import com.mohiva.play.silhouette.api.{LogoutEvent, Silhouette}
import javax.inject.Inject
import play.api.i18n.I18nSupport
import play.api.mvc.{AbstractController, AnyContent, ControllerComponents}
import repos.{DocumentRepository, UserRepository}
import utils.auth.{CustomSecuredErrorHandler, DefaultEnv, HasPermission}
import model._
import api._
import boopickle.BasicPicklers
import forms.EmailForm
import play.{Environment, Play}
import play.api.libs.json.{JsSuccess, Json}

import scala.concurrent.{ExecutionContext, Future}

class DocumentsController @Inject() (
  components: ControllerComponents,
  silhouette: Silhouette[DefaultEnv],
  users: UserRepository,
  docs: DocumentRepository,
  securedErrorHandler: CustomSecuredErrorHandler,
)(implicit assets: AssetsFinder, ex: ExecutionContext) extends AbstractController(components) with I18nSupport {

  def documents = silhouette.SecuredAction.async { implicit request =>
    docs.list(request.identity.userId).map(res => {
      Ok(views.html.documents(request.identity, res))
    })
  }

  def create = silhouette.SecuredAction { implicit  request =>
    Ok(views.html.documentCreate(request.identity))
  }

  def options(documentId: UUID) =  silhouette.SecuredAction(HasPermission[DefaultEnv#A](documentId, users, PermissionLevel.ReadOnly)).async { implicit request =>
    docs.head(request.identity.userId, documentId).flatMap {
      case Some(result) =>
        users.collabrators(request.identity.userId, documentId).flatMap { cs =>
          Future.successful(Ok(views.html.documentOptions(request.identity, result, cs)))
        }
      case None =>
        securedErrorHandler.onNotAuthorized(request)
    }
  }



  def delete(documentId: UUID) = silhouette.SecuredAction(HasPermission[DefaultEnv#A](documentId, users, PermissionLevel.Admin)).async { implicit request =>
    docs.delete(documentId).map(_ => Redirect(routes.DocumentsController.documents()).flashing("success" -> "Success!"))
  }

  def addCollabrator(documentId: UUID) = silhouette.SecuredAction(HasPermission[DefaultEnv#A](documentId, users, PermissionLevel.Admin)).async { implicit request =>
    EmailForm.form.bindFromRequest.fold(
      form => ???,
      data => {
        users.retrieve(data).flatMap {
          case Some(user) =>
            users.addPermission(documentId, user.userId, PermissionLevel.Edit).flatMap(_ => Future.successful(Redirect(routes.DocumentsController.options(documentId))))
          case None => Future.successful(Redirect(routes.DocumentsController.options(documentId)).flashing("error" -> "User not found."))
        }
      })
  }

  def deleteCollabrator(documentId: UUID) = silhouette.SecuredAction(HasPermission[DefaultEnv#A](documentId, users, PermissionLevel.Admin)).async { implicit request =>
    EmailForm.form.bindFromRequest.fold(
      form => ???,
      data => {
        users.retrieve(data).flatMap {
          case Some(user) =>
            users.deletePermission(documentId, user.userId).flatMap(_ => Future.successful(Redirect(routes.DocumentsController.options(documentId))))
          case None => Future.successful(Redirect(routes.DocumentsController.options(documentId)).flashing("error" -> "User not found."))
        }
      })
  }

  def createEmpty() = silhouette.SecuredAction.async { implicit request =>
    val node = model.data.Node.create(request.messages.apply("new.document"))
    docs.create(request.identity.userId, node).map(_ => Redirect(routes.DocumentsController.documents()))
  }

  def importJson() = silhouette.SecuredAction.async(parse.multipartFormData) { implicit request =>
    import model._
    Json.fromJson[model.data.Node](Json.parse(new FileInputStream(request.body.file("file").get.ref.file))) match {
      case JsSuccess(node, path) =>
        docs.create(request.identity.userId, node.regenerateIds()).map(_ =>
          Redirect(routes.DocumentsController.documents()).flashing("success" -> "Success!"))
      case _ =>
        ???
    }
  }


}
