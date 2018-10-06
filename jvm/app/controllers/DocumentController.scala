package controllers

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.stream.Materializer
import com.mohiva.play.silhouette.api.actions.SecuredRequest
import com.mohiva.play.silhouette.api.{HandlerResult, Silhouette}
import javax.inject.Inject
import models.User
import play.api.i18n.I18nSupport
import play.api.libs.streams.ActorFlow
import play.api.mvc._
import utils.auth.DefaultEnv

import scala.concurrent.{ExecutionContext, Future}


object MyWebSocketActor {
  def props(user: User)(out: ActorRef) = Props(new MyWebSocketActor(user, out))
}

class MyWebSocketActor(user: User, out: ActorRef) extends Actor {
  def receive = {
    case msg: String =>
      out ! (s"Hi ${user.name}, I received your message: " + msg)
  }
}


class DocumentController @Inject() (
  components: ControllerComponents,
  silhouette: Silhouette[DefaultEnv],

)(implicit assets: AssetsFinder,
  system: ActorSystem,
  materializer: Materializer,
  ec: ExecutionContext
) extends AbstractController(components) with I18nSupport {


  def index(documentId: String) = silhouette.SecuredAction.async { implicit request: SecuredRequest[DefaultEnv, AnyContent] =>
    Future.successful(Ok(views.html.editor(documentId)))
  }

  def init(documentId: String) = silhouette.SecuredAction.async { implicit request: SecuredRequest[DefaultEnv, AnyContent] =>
    Future.successful(Ok(views.html.home(request.identity)))
  }

  def changes(documentId: String) = silhouette.SecuredAction.async { implicit request: SecuredRequest[DefaultEnv, AnyContent] =>
    Future.successful(Ok(views.html.home(request.identity)))
  }

  def ws(documentId: String) = WebSocket.acceptOrResult[String, String] { request =>
    implicit val req = Request(request, AnyContentAsEmpty)
    silhouette.SecuredRequestHandler { securedRequest =>
      Future.successful(HandlerResult(Ok, Some(securedRequest.identity)))
    }.map {
      case HandlerResult(r, Some(user)) => Right(ActorFlow.actorRef(MyWebSocketActor.props(user)))
      case HandlerResult(r, None) => Left(r)
    }
  }

}
