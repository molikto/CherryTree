package shared.client


import autowire._
import com.softwaremill.quicklens._

import monix.execution.Cancelable
import monix.reactive.Observable
import monix.execution.rstreams.Subscription
import monix.reactive._

import concurrent.duration._
import monix.execution.Scheduler.Implicits.global
import shared.api._
import shared.data0._
import shared.ot.Rebased
import shared.util._

import scala.concurrent.Future
import scala.util.{Failure, Success}

trait ClientModelStateTrait { self =>

  def lockObject: AnyRef  = self
  protected def initial: ClientInit
  protected def server: Server
  protected val authentication: Authentication.Token
  def debug_authentication: Authentication.Token = authentication

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


  private var committedVersion: Int = initial.version
  private var committed: Node = initial.node
  private var uncommitted = Seq.empty[Node.Transaction]

  def debug_committedVersion = committedVersion

  /**
    * out facing state
    */
  val state = ObservableProperty(committed)

  /**
    * request queue
    */
  private var requesting = false
  private var requests: Seq[Int] = Seq.empty[Int]

  private def putBackAndMarkNotConnected(head: Int): Unit = {
    requests = head +: requests
    connected.update(false)
  }

  private def request[T](head: Int, a: Future[ErrorT[T]], onSuccess: T => Unit): Unit = {
    requesting = true
    transform(a).onComplete {
      case Success(r) =>
        self.synchronized {
          requesting = false
          onSuccess(r)
          tryTopRequest()
        }
      case Failure(t) =>
        self.synchronized {
          requesting = false
          t.printStackTrace()
          t match {
            case ApiError.ClientVersionIsOlderThanServerCache =>
            case ApiError.InvalidToken =>
              putBackAndMarkNotConnected(head)
            case _ =>
              requesting = false
              putBackAndMarkNotConnected(head)
          }
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
          request[ClientUpdate](head, server.change(authentication, committedVersion, submit).call(), succsss => {
            updateFromServer(succsss)
          })
        case _ =>
      }
    }
  }

  def updating: Boolean = requesting

  def hasUncommited: Boolean = uncommitted.nonEmpty

  /**
    * sync with remote server
    */
  def sync(): Boolean = self.synchronized {
    if (requests.isEmpty) {
      requests = requests :+ 0
    }
    if (!requesting) {
      tryTopRequest()
      true
    } else {
      false
    }
  }


  private def updateFromServer(success: ClientUpdate): Unit = {
    val take = success.acceptedLosersCount
    val winners = success.winners
    val loser = uncommitted.take(take)
    // TODO handle conflict, modal handling of winner deletes loser
    val Rebased(cs0, (wp, lp)) = Node.Ot.rebaseT(winners.flatten, loser)
    committed = Node.Ot.applyT(lp, Node.Ot.applyT(winners, committed))
    committedVersion += winners.size + take
    val Rebased(cs1, (wp0, uc)) = Node.Ot.rebaseT(wp, uncommitted.drop(take))
    uncommitted = uc
    state.modify(a => Node.Ot.apply(wp0, a))
  }


  /**
    * submit a change to local state, a sync might follow
    */
  def change(changes: Node.Transaction, sync: Boolean = true): Unit = self.synchronized {
    state.modify(a => Node.Ot.apply(changes, a))
    uncommitted = uncommitted :+ changes
    if (sync) self.sync()
  }
}
