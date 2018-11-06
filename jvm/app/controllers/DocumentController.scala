package controllers

import java.util.UUID

import akka.actor.{Actor, ActorRef, ActorSystem, OneForOneStrategy, PoisonPill, Props, Status, SupervisorStrategy, Terminated}
import akka.stream.{Materializer, OverflowStrategy}
import akka.util.{ByteString, Timeout}
import api.{ChangeRequest, ChangeResponse, InitResponse}
import com.mohiva.play.silhouette.api.actions.SecuredRequest
import com.mohiva.play.silhouette.api.{HandlerResult, Silhouette}
import javax.inject.{Inject, Singleton}
import akka.pattern.ask
import akka.stream.scaladsl.{Flow, Keep, Sink, Source}
import models.User
import play.api.http.HttpEntity
import play.api.i18n.I18nSupport
import play.api.libs.streams.ActorFlow
import play.api.mvc._
import play.filters.csrf.CSRF
import server.Server
import utils.auth.{DefaultEnv, HasPermission}
import model._
import api._
import model.operation.Node
import model.transaction.Node
import play.api.Logger
import repos.{DocumentRepository, UserRepository}

import scala.collection.mutable
import monix.execution.Scheduler.Implicits.global
import play.api.libs.json.Json

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Success, Try}
import scala.concurrent.duration._


object WebSocketActor {
  def props(userId: UUID, out: ActorRef) = Props(new WebSocketActor(userId, out))
}

class WebSocketActor(userId: UUID, out: ActorRef) extends Actor {

  override def preStart(): Unit = {
    Logger.debug("starting a WebSocket actor")
  }

  override def postStop(): Unit = {
    Logger.debug("stopping a WebSocket actor")
  }

  def receive = {
    case Status.Success(_) | Status.Failure(_) =>
      context.parent ! DocumentActor.Message.WsEnd(userId, context.self)
      context.stop(self)
    case other =>
      Logger.debug(s"sending WebSocket message $other")
      out ! other
  }
}

object DocumentActor {
  def props(id: UUID, docs: DocumentRepository) = Props(new DocumentActor(id, docs))

  sealed trait Message {
  }
  object Message {
    case class Init(user: User, i: InitRequest) extends Message
    case class Change(user: User, req: ChangeRequest) extends Message
    case class Ws(user: User, actor: ActorRef) extends Message
    case class WsEnd(user: UUID, actor: ActorRef) extends Message
  }
}
class DocumentActor(id: UUID, docs: DocumentRepository) extends Actor {

  import DocumentActor.Message

  private var server: Server = null

  private val online = new mutable.HashMap[UUID, mutable.Set[ActorRef]] with mutable.MultiMap[UUID, ActorRef]
  private var collaborators = Seq.empty[User]

  override def preStart(): Unit = {
    Logger.debug(s"starting document actor for document $id")
  }


  override def postStop(): Unit = {
    Logger.debug(s"stopping document actor for document $id")
  }

  private def ensureInit(): Unit = {
    if (server == null) {
      val root = Await.result(docs.init(id), 10.seconds)
      server = new Server(id, root._1, root._2) {
        override def persist(userId: UUID, changes: Seq[(model.transaction.Node, UUID, Seq[model.operation.Node.Diff])]): Unit = {
          Await.result(docs.changes(userId, id, version, changes), 10.seconds)
        }

        override def loadChanges(from: Int, until: Int): Option[Seq[(model.transaction.Node, UUID)]] = None

        override def serverStatus(userId: UUID): ServerStatus = ServerStatus(collaborators.filter(_.userId != userId).map(u => Collaborator(u.email, u.name)))
      }
    }
  }

  override def receive: Receive = {
    case Message.Init(user, req) =>
      ensureInit()
      sender ! server.init(user.userId)
    case Message.Change(user, req) =>
      ensureInit()
      sender ! server.change(user.userId, req)
      if (req.ts.nonEmpty) context.children.foreach(_ ! "update")
    case Message.WsEnd(user, actor) =>
      online.removeBinding(user, actor)
      if (!online.exists(_._1 == user)) {
        collaborators = collaborators.filter(_.userId != user)
      }
    case Message.Ws(user, out) =>
      val ws = context.actorOf(WebSocketActor.props(user.userId, out))
      if (!collaborators.exists(_.userId == user.userId)) collaborators = collaborators :+ user
      online.addBinding(user.userId, ws)
      sender ! ws
  }
}


class DocumentsActor(val docs: DocumentRepository) extends Actor {
  override def receive: Receive = {
    case id: UUID => sender ! context.child(id.toString).getOrElse(context.actorOf(DocumentActor.props(id, docs), id.toString))
  }
}

@Singleton
class DocumentController @Inject() (
  components: ControllerComponents,
  silhouette: Silhouette[DefaultEnv],
  users: UserRepository,
  docs: DocumentRepository
)(implicit assets: AssetsFinder,
  system: ActorSystem,
  materializer: Materializer,
  ec: ExecutionContext
) extends AbstractController(components) with I18nSupport {

  private val documents = system.actorOf(Props(new DocumentsActor(docs)))

  /**
    * html returning
    */

  def index(documentId: UUID) = silhouette.SecuredAction(HasPermission[DefaultEnv#A](documentId, users)) { implicit request =>
    Ok(views.html.editor(documentId, None))
  }

  def node(documentId: UUID, nid: UUID) = silhouette.SecuredAction(HasPermission[DefaultEnv#A](documentId, users)) { implicit request =>
    Ok(views.html.editor(documentId, Some(nid)))
  }

  def nodeInfo(documentId: UUID, nid: UUID) = silhouette.SecuredAction(HasPermission[DefaultEnv#A](documentId, users)).async { implicit request =>
    docs.nodeInfo(documentId, nid).map(a => Ok.sendEntity(toEntity(a)))
  }

  def init(documentId: UUID) = silhouette.SecuredAction(HasPermission[DefaultEnv#A](documentId, users)).async { implicit request =>
    implicit val timeout: Timeout = 1.minute
    (documents ? documentId).mapTo[ActorRef].flatMap(_ ? DocumentActor.Message.Init(request.identity, InitRequest())).mapTo[InitResponse].map { response =>
      Ok.sendEntity(toEntity(response))
    }
  }

  def json(documentId: UUID) = silhouette.SecuredAction(HasPermission[DefaultEnv#A](documentId, users)).async { implicit request =>
    implicit val timeout: Timeout = 1.minute
    (documents ? documentId).mapTo[ActorRef].flatMap(_ ? DocumentActor.Message.Init(request.identity, InitRequest())).mapTo[InitResponse].map { response =>
      Ok(Json.toJson(response.node))
    }
  }

  def changes(documentId: UUID) = silhouette.SecuredAction(HasPermission[DefaultEnv#A](documentId, users, PermissionLevel.Edit)).async(parse.byteString) { implicit request =>
    val change = fromRequest[ChangeRequest](request)
    implicit val timeout: Timeout = 1.minute
    (documents ? documentId).mapTo[ActorRef].flatMap(_ ? DocumentActor.Message.Change(request.identity, change)).mapTo[Try[ChangeResponse]].map {
      case Success(suc) =>
        Ok.sendEntity(toEntity(suc))
      case Failure(exc) =>
        ???
    }
  }

  def ws(documentId: UUID) = WebSocket.acceptOrResult[String, String] { request =>
    implicit val req = Request(request, AnyContentAsEmpty) // dummy request
    silhouette.SecuredRequestHandler(HasPermission[DefaultEnv#A](documentId, users)) { securedRequest =>
      Future.successful(HandlerResult(Ok, Some(securedRequest.identity)))
    }.flatMap {
      case HandlerResult(r, Some(user)) =>
        implicit val timeout: Timeout = 1.minute
        (documents ? documentId).mapTo[ActorRef].flatMap { a =>
            val (outActor, publisher) = Source.actorRef[String](16, OverflowStrategy.dropNew)
              .toMat(Sink.asPublisher(false))(Keep.both).run()
          (a ? DocumentActor.Message.Ws(user, outActor)).mapTo[ActorRef].map(ref => (ref, publisher))
        }.map { pair =>
          val flow = Flow.fromSinkAndSource(
            Sink.actorRef(pair._1, akka.actor.Status.Success(())),
            Source.fromPublisher(pair._2)
          )
          Right(flow)
        }
      case HandlerResult(r, None) => Future.successful(Left(r))
    }
    // TODO recover here
  }

}
