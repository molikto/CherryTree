package client


import java.nio.ByteBuffer

import autowire._
import com.softwaremill.quicklens._
import monix.execution.Cancelable
import monix.reactive.Observable
import monix.execution.rstreams.Subscription
import monix.reactive._

import concurrent.duration._
import monix.execution.Scheduler.Implicits.global
import api._
import command.{Commands, Key}
import model.ot.Rebased
import util._
import model._
import model.data.{SpecialChar, Unicode}
import monix.reactive.subjects.PublishSubject

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.Future
import scala.util.{Failure, Success}


object Client {
  type Proxy = ClientProxy[Api, ByteBuffer, boopickle.Pickler, boopickle.Pickler]

  case class Update(
    transaction: model.transaction.Node,
    mode: Option[model.mode.Node],
    viewUpdated: Boolean = false)

  case class UpdateResult(
    root: model.data.Node,
    transaction: model.transaction.Node,
    mode: Option[model.mode.Node],
    viewUpdated: Boolean = false)
}

class Client(
  private val server: Client.Proxy,
  private val initial: ClientInit,
  private val authentication: Authentication.Token
) extends Commands { self =>

  protected def lockObject: AnyRef  = self
  def debug_authentication: Authentication.Token = authentication

  /**
    * connection state
    */
  val connected = ObservableProperty(true)

  val errors: ObservableProperty[Option[Throwable]] = ObservableProperty(None)


  private var subscription: Cancelable = null

  def start(): Unit = {
    // LATER make this an option. so it is easier to debug...
    subscription = Observable.interval(5000.millis).doOnNext(_ => sync()).subscribe()
  }

  def stop(): Unit = {
    if (subscription != null) {
      subscription.cancel()
      subscription = null
    }
  }


  private var committedVersion: Int = initial.version
  def debug_committedVersion = committedVersion
  private var committed: data.Node = initial.node
  def debug_committed = committed
  private var uncommitted = Seq.empty[transaction.Node]
  def version: Int = committedVersion + uncommitted.size

  /**
    * document observable
    *
    * editor queue
    */
  private var state_ = ClientState(committed, Some(initial.mode))

  private val flushes_ : PublishSubject[Unit] = PublishSubject[Unit]()

  private var disableStateUpdate_ : Boolean = false

  private var flushing = false
  private var updatingState = false
  def disableStateUpdate: Boolean = disableStateUpdate_
  def disableStateUpdate_=(a: Boolean): Unit = {
    if (flushing) throw new IllegalStateException("You should not change state will fetching state!!")
    disableStateUpdate_ = a
    if (!a) {
      flush()
    }
  }

  def flushes: Observable[Unit] = flushes_

  def flush(): Unit = {
    if (!disableStateUpdate) {
      flushing = true
      flushes_.onNext(Unit)
      flushing = false
      disabledStateUpdates.foreach(a => {
        updateInner(a)
      })
      disabledStateUpdates.clear()
    }
  }



  private val disabledStateUpdates = ArrayBuffer[Client.UpdateResult]()

  private val stateUpdates_ : PublishSubject[Client.UpdateResult] = PublishSubject[Client.UpdateResult]()

  def stateUpdates: Observable[Client.UpdateResult] = stateUpdates_

  private var insertingFlusher: Cancelable = null

  private def updateInner(res: Client.UpdateResult): Unit = {
    if (updatingState) throw new IllegalStateException("You should not update state during a state update!!!")
    println("client update inner: " + res)
    updatingState = true
    state_ = ClientState(res.root, res.mode)
    onBeforeUpdateUpdateCommandState(state_)
    stateUpdates_.onNext(res)
    updatingState = false
    if (state_.isRichInserting) {
      if (insertingFlusher == null) {
        insertingFlusher = Observable.interval(300.millis).doOnNext(_ => flush()).subscribe()
      }
    } else {
      if (insertingFlusher != null) {
        insertingFlusher.cancel()
        insertingFlusher = null
      }
    }
  }

  private def updateState(a: ClientState, from: model.transaction.Node, viewUpdated: Boolean): Unit = {
    val res = Client.UpdateResult(a.node, from, a.mode, viewUpdated)
    // the queued updates is NOT applied in this method, instead they are applied after any flush!!!
    if (disableStateUpdate_) {
      disabledStateUpdates.append(res)
    } else {
      updateInner(res)
    }
  }
  def state: ClientState = state_

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
          errors.update(None)
        }
      case Failure(t) =>
        self.synchronized {
          requesting = false
          errors.update(Some(t))
          t match {
            case ApiError.ClientVersionIsOlderThanServerCache =>
              // LATER properly handle this!
              putBackAndMarkNotConnected(head)
            case ApiError.InvalidToken =>
              // LATER properly handle this!
              putBackAndMarkNotConnected(head)
            case _ =>
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
          request[ClientUpdate](head, server.change(authentication, committedVersion, submit, state.mode, if (debugModel) committed else data.Node.empty).call(), succsss => {
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


  /**
    * update committed, committed version, uncommited, state
    */
  private def updateFromServer(success: ClientUpdate): Unit = {
    try {
      val take = success.acceptedLosersCount
      val winners = success.winners
      val (loser, remaining) = uncommitted.splitAt(take)
      // LATER handle conflict, modal handling of winner deletes loser
      val Rebased(cs0, (wp, lp)) = ot.Node.rebaseT(winners.flatten, loser)
      committed = operation.Node.applyT(lp, operation.Node.applyT(winners, committed))
      val altVersion = committedVersion + winners.size + lp.size
      committedVersion = success.finalVersion
      assert(altVersion == committedVersion, s"Version wrong! $committedVersion $altVersion ${winners.size} $take")
      val Rebased(cs1, (wp0, uc)) = ot.Node.rebaseT(wp, remaining)
      uncommitted = uc
      if (wp0.nonEmpty) updateState(
        ClientState(operation.Node.apply(wp0, state.node), state.mode.flatMap(a => operation.Node.transform(wp0, a))), wp0, viewUpdated = false)
    } catch {
      case e: Exception =>
        throw new Exception(s"Apply update from server failed $success #### $committed", e)
    }
  }


  def act(command: Command): Unit = {
    flush()
    change(command.action(state))
  }

  // LATER updatable keymap
  private val keyToCommand: Map[Key, Seq[Command]] = commands.flatMap(c => c.keys.map(_ -> c)).groupBy(_._1).map(a => (a._1, a._2.map(_._2)))
  private val graphemeToCommand: Map[Key.Grapheme, Seq[Command]] = commands.flatMap(
    c => c.keys.filter(k => !k.control && !k.meta && k.a.isInstanceOf[Key.Grapheme]
    ).map(_.a.asInstanceOf[Key.Grapheme] -> c)).groupBy(_._1).map(a => (a._1, a._2.map(_._2))).toMap


  def keyDown(key: Key): Boolean = {
    if (isWaitingForGraphemeCommand) {
      key.a match {
        case g@Key.Grapheme(a) => change(consumeByWaitingForGraphemeCommand(state, g))
        case _: Key.Modifier => // ignore modifier only keys
        case _ => clearWaitingForGraphemeCommand()
      }
      true
    } else {
      def doCommand(): Boolean = {
        val av = keyToCommand.getOrElse(key, Seq.empty).filter(_.available(state))
        if (av.nonEmpty) {
          if (av.size > 1) errors.update(Some(new Exception("Multiple commands with same key")))
          act(av.head)
          true
        } else {
          if (!key.control && !key.meta && key.a.isInstanceOf[Key.Grapheme]) {
            val av = graphemeToCommand.getOrElse(key.a.asInstanceOf[Key.Grapheme], Seq.empty).filter(_.available(state))
            if (av.nonEmpty) {
              if (av.size > 1) errors.update(Some(new Exception("Multiple commands with same key")))
              act(av.head)
              true
            } else {
              false
            }
          } else {
            false
          }
        }
      }
      if (state.isRichInserting) {
        // some keys we MUST keep
        doCommand()
      } else {
        doCommand()
        true
      }
    }
  }


  /**
    * view calls this method to insert text at current insertion point,
    */
  def onInsertRichTextAndViewUpdated(unicode: Unicode): Unit = {
    import model._
    val (n, _, insert) = state.asRichInsert
    change(
      Seq(operation.Node.Content(n, operation.Content.Rich(operation.Rich.insert(insert.pos, unicode)))),
      Some(state.copyContentMode(mode.Content.RichInsert(insert.pos + unicode.size))),
      viewUpdated = true)
  }


  def change(update: Client.Update): Unit = change(update.transaction, update.mode, viewUpdated = update.viewUpdated)
  /**
    * submit a change to local state, a sync might follow
    *
    * if mode is defined, it is explicitly set **after** the change is applied
    * if not, the mode is transformed by the change
    */
  def change(changes: transaction.Node,
    mode: Option[model.mode.Node] = None,
    viewUpdated: Boolean = false,
    sync: Boolean = true): Unit = self.synchronized {
    var changed = false
    val d = if (changes.nonEmpty) {
      changed = true
      operation.Node.apply(changes, state.node)
    } else {
      state.node
    }
    val m = mode match {
      case Some(_) =>
        changed = true
        mode
      case None =>
        if (changed) {
          state.mode.flatMap(a => operation.Node.transform(changes, a))
        } else {
          state.mode
        }
    }
    if (changed) {
      updateState(ClientState(d, m), changes, viewUpdated = viewUpdated)
      uncommitted = uncommitted :+ changes
      if (sync) self.sync()
    }
  }

  /**
    * these are settings??
    */
  override def delimitationCodePoints: Map[SpecialChar.Delimitation, Unicode] = Map(
    SpecialChar.StrikeThrough -> Unicode("-"),
    SpecialChar.Code -> Unicode("`"),
    SpecialChar.Strong -> Unicode("#"),
    SpecialChar.LaTeX -> Unicode("$"),
    SpecialChar.Link -> Unicode("["),
    SpecialChar.Emphasis -> Unicode("*")
  )

  override def additionalKeyMaps: Map[String, Seq[Key]] = Map.empty

  override def removedDefaultKeyMaps: Map[String, Seq[Key]] = Map.empty
}
