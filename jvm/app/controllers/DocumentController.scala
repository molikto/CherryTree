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
import utils.auth.DefaultEnv
import model._
import api._
import play.api.Logger

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}
import scala.concurrent.duration._


object DocumentActor {
  def props(id: String) = Props(new DocumentActor(id))
}
class DocumentActor(id: String) extends Actor {

  val tempServer = new Server("tempNothing")

  override def preStart(): Unit = {
    Logger.debug(s"starting actor for document $id")
  }

  override def receive: Receive = {
    case i: InitRequest =>
      sender ! tempServer.init()
    case c: ChangeRequest =>
      sender ! tempServer.change(c)
      if (c.ts.nonEmpty) context.children.foreach(_ ! "update")
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


class DocumentsActor extends Actor {
  override def receive: Receive = {
    case id: String => sender ! context.child(id).getOrElse(context.actorOf(DocumentActor.props(id), id))
  }
}

@Singleton
class DocumentController @Inject() (
  components: ControllerComponents,
  silhouette: Silhouette[DefaultEnv],

)(implicit assets: AssetsFinder,
  system: ActorSystem,
  materializer: Materializer,
  ec: ExecutionContext
) extends AbstractController(components) with I18nSupport {

  private val documents = system.actorOf(Props(new DocumentsActor()))

  def index(documentId: String) = silhouette.SecuredAction { implicit request: SecuredRequest[DefaultEnv, AnyContent] =>
    Ok(views.html.editor(documentId))
  }

  def init(documentId: String) = silhouette.SecuredAction.async { implicit request: SecuredRequest[DefaultEnv, AnyContent] =>
    implicit val timeout: Timeout = 1.minute
    //val bytes = request.body.asRaw.get.asBytes(parse.UNLIMITED).get
    //val init = Unpickle[InitRequest](implicitly).fromBytes(bytes.toByteBuffer)(unpickleState)
    val init = InitRequest()
    (documents ? documentId).mapTo[ActorRef].flatMap(_ ? init).mapTo[InitResponse].map { response =>
      val res = Pickle.intoBytes[InitResponse](response)(pickleState, implicitly)
      Ok.sendEntity(HttpEntity.Strict(ByteString(res), None))
    }
  }

  def changes(documentId: String) = silhouette.SecuredAction.async { implicit request: SecuredRequest[DefaultEnv, AnyContent] =>
    implicit val timeout: Timeout = 1.minute
    var bytes: ByteString = null
    if (model.debug_transmit) {
      println(request.body)
      println(request.body.getClass.getCanonicalName)
      bytes = request.body.asRaw.get.asBytes(parse.UNLIMITED).get
      println(bytes.mkString(","))
    }
    if (bytes == null) bytes = request.body.asRaw.get.asBytes(parse.UNLIMITED).get
    val change = Unpickle[ChangeRequest](implicitly).fromBytes(bytes.toByteBuffer)(unpickleState)
    (documents ? documentId).mapTo[ActorRef].flatMap(_ ? change).mapTo[Try[ChangeResponse]].map {
      case Success(suc) =>
        val res = Pickle.intoBytes[ChangeResponse](suc)(pickleState, implicitly)
        Ok.sendEntity(HttpEntity.Strict(ByteString(res), None))
      case Failure(exc) =>
        Ok("")
    }
  }

  def ws(documentId: String) = WebSocket.acceptOrResult[String, String] { request =>
    implicit val req = Request(request, AnyContentAsEmpty) // dummy request
    silhouette.SecuredRequestHandler { securedRequest =>
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