package client

import com.softwaremill.quicklens._
import shared.data._
import autowire._
import client.net.AutowireServer
import boopickle.Default._
import org.scalajs.dom

import scala.scalajs.js
import autowire._
import boopickle.Default._
import client.net.AutowireServer

import scala.util.{Failure, Success}
import scala.concurrent.ExecutionContext.Implicits.global
import org.scalajs.dom
import rxscalajs.{Observable, Subject}
import rxscalajs.subjects.BehaviorSubject
import shared._
import shared.data._

import scala.concurrent.Future
import scala.util.Success

class Client(initial: ClientState) {

  /**
    * connection state
    */
  val connected = ObservableProperty(true)

  /**
    * out facing state
    */
  val state = ObservableProperty(initial)

  private var committed: ClientState = initial
  private var uncommitted = Seq.empty[Transaction]

  /**
    * request queue
    */
  private var requesting = false
  private var requests: Seq[Unit] = Seq.empty

  private def putAndDo(work: Unit): Unit = {
  }

  def putBackAndMarkNotConnected(head: Unit) = {
    requests = head +: requests
    connected.update(false)
  }

  private def request[T](head: Unit, a: Future[ErrorT[T]], onSuccess: T => Unit): Unit = {
    requesting = true
    a.onComplete {
      case Success(Right(r)) =>
        requesting = false
        onSuccess(r)
        tryTopRequest()
      case Success(Left(error)) =>
        requesting = false
        error match {
          case ApiError.ClientVersionIsOlderThanServerCache =>
            // ignore this
            tryTopRequest()
          case ApiError.InvalidToken =>
            putBackAndMarkNotConnected(head)
        }
      case Failure(t) =>
        requesting = false
        putBackAndMarkNotConnected(head)
    }
  }


  private def tryTopRequest(): Unit = {
    if (!requesting) {
      val head: Unit = requests.head
      requests = requests.tail
      request[ClientStateUpdate](head, api.change(ClientStateSnapshot(committed), uncommitted).call(), updateFromServer)
    }
  }


  private def sync(): Unit = {
    if (requests.isEmpty) {
      requests = requests :+ Unit
      tryTopRequest()
    }
  }


  private def updateFromServer(success: ClientStateUpdate) = {
    val take = success.document.acceptedLosersCount
    val winners = success.document.winners
    val loser = uncommitted.take(take)
    val (wp, lp) = Transaction.rebase(winners, loser, RebaseConflict.all)
    val doc = committed.document.modify(_.root).using { a => Transaction.apply(Transaction.apply(a, winners), lp)}
        .modify(_.version).using(_ + winners.size + take)
    committed = committed.copy(document = doc)
    // TODO handle rebase error
    uncommitted = Transaction.rebase(Seq(Transaction(wp)), uncommitted.drop(take), RebaseConflict.all)._2
  }


  def change(changes: Transaction): Unit = {
    state.update(state.get.modify(_.document.root).using(r => Transaction.apply(r, Seq(changes))))
    uncommitted = uncommitted :+ changes
    sync()
  }
}
