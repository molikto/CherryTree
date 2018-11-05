package controllers

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
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Success, Try}
import scala.concurrent.duration._


object DocumentActor {
  def props(id: String, docs: DocumentRepository) = Props(new DocumentActor(id, docs))
}
class DocumentActor(id: String, docs: DocumentRepository) extends Actor {

  private var server: Server = null

  override def preStart(): Unit = {
    Logger.debug(s"starting actor for document $id")
  }

  override def receive: Receive = {
    case i: InitRequest =>
      if (server == null) {
        val root = Await.result(docs.init(id), 10.seconds)
        server = new Server(id, root._1, root._2) {
          override def persist(userId: String, changes: Seq[(model.transaction.Node, Seq[model.operation.Node.Diff])]): Unit = {
            Await.result(docs.changes(userId, id, version, changes), 10.seconds)
          }

          override def loadChanges(from: Int, until: Int): Option[Seq[model.transaction.Node]] = None
        }
      }
      sender ! server.init()
    case c: (User, ChangeRequest) =>
      sender ! server.change(c._1.userId, c._2)
      if (c._2.ts.nonEmpty) context.children.foreach(_ ! "update")
    case out: ActorRef =>
      sender ! context.actorOf(Props(new Actor {


        override def preStart(): Unit = {
          Logger.debug("starting a WebSocket actor")
        }

        override def postStop(): Unit = {
          Logger.debug("stopping a WebSocket actor")
        }

        def receive = {
          case Status.Success(_) | Status.Failure(_) => context.stop(self)
          case other =>
            Logger.debug(s"sending websocket message $other")
            out ! other
        }
      }))
  }
}


class DocumentsActor(val docs: DocumentRepository) extends Actor {
  override def receive: Receive = {
    case id: String => sender ! context.child(id).getOrElse(context.actorOf(DocumentActor.props(id, docs), id))
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

  def index(documentId: String) = silhouette.SecuredAction(HasPermission[DefaultEnv#A](documentId, users)) { implicit request =>
    Ok(views.html.editor(documentId, ""))
  }

  def node(documentId: String, nid: String) = silhouette.SecuredAction(HasPermission[DefaultEnv#A](documentId, users)) { implicit request =>
    Ok(views.html.editor(documentId, nid))
  }

  def nodeInfo(documentId: String, nid: String) = silhouette.SecuredAction(HasPermission[DefaultEnv#A](documentId, users)).async { implicit request =>
    docs.nodeInfo(documentId, nid).map(a => Ok.sendEntity(toEntity(a)))
  }

  def init(documentId: String) = silhouette.SecuredAction(HasPermission[DefaultEnv#A](documentId, users)).async { implicit request =>
    implicit val timeout: Timeout = 1.minute
    (documents ? documentId).mapTo[ActorRef].flatMap(_ ? InitRequest()).mapTo[InitResponse].map { response =>
      Ok.sendEntity(toEntity(response))
    }
  }

  def changes(documentId: String) = silhouette.SecuredAction(HasPermission[DefaultEnv#A](documentId, users, PermissionLevel.Edit)).async(parse.byteString) { implicit request =>
    val change = fromRequest[ChangeRequest](request)
    implicit val timeout: Timeout = 1.minute
    (documents ? documentId).mapTo[ActorRef].flatMap(_ ? (request.identity, change)).mapTo[Try[ChangeResponse]].map {
      case Success(suc) =>
        Ok.sendEntity(toEntity(suc))
      case Failure(exc) =>
        ???
    }
  }

  def ws(documentId: String) = WebSocket.acceptOrResult[String, String] { request =>
    implicit val req = Request(request, AnyContentAsEmpty) // dummy request
    silhouette.SecuredRequestHandler(HasPermission[DefaultEnv#A](documentId, users)) { securedRequest =>
      Future.successful(HandlerResult(Ok, Some(securedRequest.identity)))
    }.flatMap {
      case HandlerResult(r, Some(user)) =>
        implicit val timeout: Timeout = 1.minute
        (documents ? documentId).mapTo[ActorRef].flatMap { a =>
          // TODO stop the actor created in Source.actorRef
            val (outActor, publisher) = Source.actorRef[String](16, OverflowStrategy.dropNew)
              .toMat(Sink.asPublisher(false))(Keep.both).run()
          (a ? outActor).mapTo[ActorRef].map(ref => (ref, publisher))
        }.map { pair =>
          val flow = Flow.fromSinkAndSource(
            Sink.actorRef(pair._1, akka.actor.Status.Success(())),
            Source.fromPublisher(pair._2)
          )
          Right(flow)
        }
      case HandlerResult(r, None) => Future.successful(Left(r))
    }
  }

}
