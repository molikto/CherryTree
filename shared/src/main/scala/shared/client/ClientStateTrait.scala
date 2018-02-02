package shared.client


import autowire._
import com.softwaremill.quicklens._
import shared.data._
import boopickle.Default._
import monix.execution.Cancelable
import monix.reactive.Observable
import monix.execution.rstreams.Subscription
import monix.reactive._

import concurrent.duration._
import monix.execution.Scheduler.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}

trait ClientStateTrait { self =>

  def lockObject: AnyRef  = self
  protected def initial: ClientState
  protected def server: Server

  /**
    * connection state
    */
  val connected = ObservableProperty(true)


  private var subscription: Cancelable = null

  def start(): Unit = {
    subscription = Observable.interval(300.millis).doOnNext(_ => sync()).subscribe()
  }

  def stop(): Unit = {
    if (subscription != null) {
      subscription.cancel()
      subscription = null
    }
  }
  /**
    * out facing state
    */
  val state = ObservableProperty(initial)

  private var committed: ClientState = initial
  private var uncommitted = Seq.empty[Transaction]
  def debugCommitted: ClientState = committed

  /**
    * request queue
    */
  private var requesting = false
  private var requests: Seq[Unit] = Seq.empty

  private def putBackAndMarkNotConnected(head: Unit): Unit = {
    requests = head +: requests
    connected.update(false)
  }

  private def request[T](head: Unit, a: Future[ErrorT[T]], onSuccess: T => Unit): Unit = {
    requesting = true
    a.onComplete {
      case Success(Right(r)) =>
        self.synchronized {
          requesting = false
          onSuccess(r)
          tryTopRequest()
        }
      case Success(Left(error)) =>
        self.synchronized {
          requesting = false
          new IllegalStateException(error.toString).printStackTrace()
          error match {
            case ApiError.ClientVersionIsOlderThanServerCache =>
              // ignore this
              tryTopRequest()
            case ApiError.InvalidToken =>
              putBackAndMarkNotConnected(head)
          }
        }
      case Failure(t) =>
        self.synchronized {
          t.printStackTrace()
          requesting = false
          putBackAndMarkNotConnected(head)
        }
    }
  }


  private def tryTopRequest(): Unit = {
    if (!requesting) {
      connected.update(true)
      requests match {
        case head :: tail =>
          requests = tail
          val submit = uncommitted
          request[ClientStateUpdate](head, server.change(ClientStateSnapshot(committed), submit).call(), succsss => {
            updateFromServer(succsss)
          })
        case _ =>
      }
    }
  }

  def updating: Boolean = requesting
  def hasUncommited: Boolean = uncommitted.nonEmpty

  def sync(): Boolean = self.synchronized {
    if (requests.isEmpty) {
      requests = requests ++ Seq[Unit](Unit)
      tryTopRequest()
      true
    } else {
      if (!requesting) {
        throw new IllegalStateException("Not possible")
      }
      false
    }
  }


  private def updateFromServer(success: ClientStateUpdate): Unit = {
    val take = success.document.acceptedLosersCount
    val winners = success.document.winners
    val loser = uncommitted.take(take)
    // TODO modal handling of winner deletes loser
    val (wp, lp) = Transaction.rebase(winners, loser, RebaseConflict.all)
    val doc = committed.document.modify(_.root).using { a => Transaction.apply(Transaction.apply(a, winners), lp)}
      .modify(_.version).using(_ + winners.size + take)
    committed = committed.copy(document = doc)
    val (wp0, uc) = Transaction.rebase(Seq(Transaction(wp)), uncommitted.drop(take), RebaseConflict.all)
    uncommitted = uc
    state.modify(_.modify(_.document.root).using(a => Transaction.apply(a, Transaction(wp0))))
  }


  def change(changes: Transaction): Unit = self.synchronized {
    state.modify(_.modify(_.document.root).using(a => Transaction.apply(a, changes)))
    uncommitted = uncommitted :+ changes
    sync()
  }

}
