package controllers

import java.nio.ByteBuffer
import java.nio.file.Files
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
import boopickle.BasicPicklers

import scala.concurrent.{ExecutionContext, Future}

class DocumentsController @Inject() (
  components: ControllerComponents,
  silhouette: Silhouette[DefaultEnv],
  docs: DocumentRepository
)(implicit assets: AssetsFinder, ex: ExecutionContext) extends AbstractController(components) with I18nSupport {


  def home = silhouette.SecuredAction { implicit  request =>
    Ok(views.html.home(request.identity))
  }

  def uploadBoopickle = silhouette.SecuredAction(parse.multipartFormData).async { implicit request =>
    request.body.file("file").map { file =>
      val bytes = Files.readAllBytes(file.ref.path)
      val node = Unpickle[model.data.Node](implicitly).fromBytes(ByteBuffer.wrap(bytes))(boopickle.Default.Unpickle)
      docs.create(request.identity.userId, node).map(_ =>
        Redirect(routes.DocumentsController.home()).flashing("success" -> "Success!"))
    }.getOrElse {
      Future.successful(Redirect(routes.DocumentsController.home()).flashing(
        "error" -> "Missing Boopickle file"))
    }
  }

  def documents = silhouette.SecuredAction.async { implicit request =>
    docs.list(request.identity.userId).map(res => {
      Ok.sendEntity(toEntity(res))
    })
  }

}
