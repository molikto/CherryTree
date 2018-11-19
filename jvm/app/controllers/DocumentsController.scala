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
import utils.auth.{DefaultEnv, HasPermission}
import model._
import api._
import boopickle.BasicPicklers
import play.{Environment, Play}
import play.api.libs.json.{JsSuccess, Json}

import scala.concurrent.{ExecutionContext, Future}

class DocumentsController @Inject() (
  components: ControllerComponents,
  silhouette: Silhouette[DefaultEnv],
  users: UserRepository,
  docs: DocumentRepository
)(implicit assets: AssetsFinder, ex: ExecutionContext) extends AbstractController(components) with I18nSupport {


  def home = silhouette.SecuredAction { implicit  request =>
    Ok(views.html.home(request.identity))
  }

  def create = silhouette.SecuredAction.async(parse.multipartFormData) { implicit request =>
    val node = model.data.Node.create(request.messages.apply("new.document"))
    docs.create(request.identity.userId, node).map(_ => Redirect(routes.DocumentController.index(node.uuid)))
  }

  def delete(documentId: UUID) = silhouette.SecuredAction(HasPermission[DefaultEnv#A](documentId, users, PermissionLevel.Admin)).async { implicit request =>
    docs.delete(documentId).map(_ => Redirect(routes.DocumentsController.home()).flashing("success" -> "Success!"))
  }

  def json = silhouette.SecuredAction.async(parse.multipartFormData) { implicit request =>
    import model._
    Json.fromJson[model.data.Node](Json.parse(new FileInputStream(request.body.file("file").get.ref.file))) match {
      case JsSuccess(node, path) =>
        docs.create(request.identity.userId, node.regenerateIds()).map(_ =>
          Redirect(routes.DocumentsController.home()).flashing("success" -> "Success!"))
      case _ =>
        ???
    }
  }

  def documents = silhouette.SecuredAction.async { implicit request =>
    docs.list(request.identity.userId).map(res => {
      Ok.sendEntity(toEntity(res))
    })
  }

}
