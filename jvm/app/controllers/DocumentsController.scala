package controllers

import java.io.FileInputStream
import java.nio.ByteBuffer
import java.nio.file.Files
import java.util.UUID

import com.mohiva.play.silhouette.api.actions.SecuredRequest
import com.mohiva.play.silhouette.api.{LogoutEvent, Silhouette}
import javax.inject.Inject
import play.api.i18n.{I18nSupport, Messages}
import play.api.mvc.{AbstractController, AnyContent, ControllerComponents}
import repos.{DocumentRepository, UserRepository}
import utils.auth.{CustomSecuredErrorHandler, DefaultEnv, HasPermission}
import model._
import api._
import boopickle.BasicPicklers
import forms.{CollabratorForm, EmailForm}
import play.{Environment, Play}
import play.api.libs.json.{JsSuccess, Json}
import play.api.libs.mailer.{Email, MailerClient}

import scala.concurrent.{ExecutionContext, Future}

class DocumentsController @Inject() (
  components: ControllerComponents,
  silhouette: Silhouette[DefaultEnv],
  users: UserRepository,
  docs: DocumentRepository,
  securedErrorHandler: CustomSecuredErrorHandler,
  mailerClient: MailerClient
)(implicit assets: AssetsFinder, ex: ExecutionContext) extends AbstractController(components) with I18nSupport {

  def documents = silhouette.SecuredAction.async { implicit request =>
    docs.list(request.identity.userId).map(res => {
      Ok(views.html.documents(request.identity, res))
    })
  }

  def create = silhouette.SecuredAction { implicit  request =>
    Ok(views.html.documentCreate(request.identity))
  }

  def options(documentId: UUID) =  silhouette.SecuredAction(HasPermission[DefaultEnv#A](documentId, users, PermissionLevel.Read)).async { implicit request =>
    docs.head(request.identity.userId, documentId).flatMap {
      case Some(result) =>
        users.collabrators(request.identity.userId, documentId).flatMap { cs =>
          Future.successful(Ok(views.html.documentOptions(request.identity, result, cs)))
        }
      case None =>
        securedErrorHandler.onNotAuthorized(request)
    }
  }



  def delete(documentId: UUID) = silhouette.SecuredAction(HasPermission[DefaultEnv#A](documentId, users, PermissionLevel.Owner)).async { implicit request =>
    docs.delete(documentId).map(_ => Redirect(routes.DocumentsController.documents()).flashing("success" -> "Success!"))
  }

  def addCollabrator(documentId: UUID) = silhouette.SecuredAction(HasPermission[DefaultEnv#A](documentId, users, PermissionLevel.Admin)).async { implicit request => {
    def fail(s: String) = Redirect(routes.DocumentsController.options(documentId)).flashing("error" -> s)
    CollabratorForm.form.bindFromRequest.fold(
      form => ???,
      data => {
        val permissionLevel = data.level.toInt
        if (permissionLevel <= PermissionLevel.Admin && permissionLevel > 0) {
          users.retrieve(data.email).flatMap {
            case Some(user) =>
              users.addPermission(documentId, user.userId, permissionLevel).flatMap(_ => {
                docs.head(request.identity.userId, documentId)
              }).map(res => {
                res match {
                  case Some(li) =>
                    val url = controllers.routes.DocumentController.index(documentId).absoluteURL()
                    mailerClient.send(Email(
                      subject =  Messages("email.new.collabrator.subject"),
                      from = Messages("email.from"),
                      to = Seq(data.email),
                      bodyText = Some(views.txt.emails.newCollabrator(user, request.identity, li.title, url).body),
                      bodyHtml = Some(views.html.emails.newCollabrator(user, request.identity, li.title, url).body)
                    ))
                  case _ =>
                }
                Redirect(routes.DocumentsController.options(documentId))
              })
            case None => Future.successful(fail("User not found"))
          }
        } else {
          Future.successful(fail("Invalid permission level"))
        }
      })
  }
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
