package controllers

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.stream.Materializer
import akka.util.ByteString
import api.{ChangeRequest, ClientInit, ClientUpdate}
import com.mohiva.play.silhouette.api.actions.SecuredRequest
import com.mohiva.play.silhouette.api.{HandlerResult, Silhouette}
import javax.inject.Inject
import models.User
import play.api.http.HttpEntity
import play.api.i18n.I18nSupport
import play.api.libs.streams.ActorFlow
import play.api.mvc._
import play.filters.csrf.CSRF
import server.Server
import utils.auth.DefaultEnv
import model._
import api._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}


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



  val tempServer = new Server("tempNothing")
  import model._

  def index(documentId: String) = silhouette.SecuredAction { implicit request: SecuredRequest[DefaultEnv, AnyContent] =>
    Ok(views.html.editor(documentId))
  }

  def init(documentId: String) = silhouette.SecuredAction { implicit request: SecuredRequest[DefaultEnv, AnyContent] =>
    val res = Pickle.intoBytes[ClientInit](tempServer.init())(implicitly, implicitly)
    Ok.sendEntity(HttpEntity.Strict(ByteString(res), None))
  }

  def changes(documentId: String) = silhouette.SecuredAction { implicit request: SecuredRequest[DefaultEnv, AnyContent] =>
    var bytes: ByteString = null
    if (model.debug_transmit) {
      println(request.body)
      println(request.body.getClass.getCanonicalName)
      bytes = request.body.asRaw.get.asBytes(parse.UNLIMITED).get
      println(bytes.mkString(","))
    }
    if (bytes == null) bytes = request.body.asRaw.get.asBytes(parse.UNLIMITED).get
    val change = Unpickle[ChangeRequest](implicitly)(bytes.toByteBuffer)
    tempServer.change(change) match {
      case Success(suc) =>
        val res = Pickle.intoBytes[ClientUpdate](suc)(implicitly, implicitly)
        Ok.sendEntity(HttpEntity.Strict(ByteString(res), None))
      case Failure(exc) =>
        Ok("")
    }
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
