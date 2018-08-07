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
import command.Key.KeySeq
import model.ot.Rebased
import util._
import model._
import model.data.{SpecialChar, Unicode}
import command._
import doc.{DocInterface, DocState, DocTransaction, DocUpdate}
import model.cursor.Node
import model.range.IntRange
import monix.reactive.subjects.PublishSubject
import register.RegisterHandler
import undoer.Undoer
import view.EditorInterface

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.Future
import scala.util.{Failure, Success}


object Client {
  type Proxy = ClientProxy[Api, ByteBuffer, boopickle.Pickler, boopickle.Pickler]



  sealed trait ViewMessage {
  }
  object ViewMessage {
    case class VisitUrl(url: String) extends ViewMessage
    case class ShowCommandMenu() extends ViewMessage
    case class ShowUrlAndTitleAttributeEditor(node: cursor.Node, range: IntRange, text: data.Text.Delimited) extends ViewMessage
    //case class ContinueCommandMenu(items: Seq[String]) extends ViewMessage
    case object ScrollToTop extends ViewMessage
    case object ScrollToBottom extends ViewMessage
  }
}

class Client(
  private val docId: String,
  private val server: Client.Proxy,
  private val initial: ClientInit,
  private val authentication: Authentication.Token
) extends CommandHandler
  with RegisterHandler
  with Undoer
  with EditorInterface
  with DocInterface { self =>


  protected def lockObject: AnyRef  = self
  def debug_authentication: Authentication.Token = authentication


  /**
    * connection state
    */
  private val connection_ = ObservableProperty[Option[ServerStatus]](Some(initial.serverStatus))

  def connection: Observable[Option[ServerStatus]] = connection_

  val errors_ : ObservableProperty[Option[Throwable]] = ObservableProperty(None)

  /**
    * last error
    */
  def errors: Observable[Option[Throwable]] = errors_

  protected val viewMessages_ : PublishSubject[Client.ViewMessage] = PublishSubject()

  def viewMessages: Observable[Client.ViewMessage] = viewMessages_



  private var subscription: Cancelable = null

  def start(): Unit = {
    subscription = Observable.interval(1000.millis).doOnNext(_ => sync()).subscribe()
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
  private var state_ = DocState(committed, Some(initial.mode), localStorage.get(docId + ".folded") match {
    case Some(s) => s.split(",").toSet
    case _ => Set.empty
  })

  /**
    * in some case the current mode conflicts with other collaborator operation, what we do is render current
    * mode as invalid, then reset to a default mode after some time,
    */
  private var modeTemp: mode.Node = null

  private val flushes_ : PublishSubject[Unit] = PublishSubject[Unit]()

  private var disableStateUpdate_ : Boolean = false

  private var disableUpdateBecauseLocalNodeDelete: (operation.Node.Delete, Long, Seq[data.Node], DocState) = null

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
        updateFromServer(a)
      })
      disabledStateUpdates.clear()
    }
  }



  private val disabledStateUpdates = ArrayBuffer[ClientUpdate]()

  private val stateUpdates_ : PublishSubject[DocUpdate] = PublishSubject[DocUpdate]()

  def stateUpdates: Observable[DocUpdate] = stateUpdates_

  private var insertingFlusher: Cancelable = null

  private var scheduledUpdateTempMode: Cancelable = null

  private def updateState(a: DocState,
    modeTemp: mode.Node,
    from: model.transaction.Node,
    ty: Undoer.Type,
    viewUpdated: Boolean,
    viewFrom: model.transaction.Node,
    fromUser: Boolean,
    foldBefore: Map[cursor.Node, Boolean] = Map.empty
  ): Unit = {
    assert(a.mode.isDefined || modeTemp != null)
    val res = DocUpdate(a.node, viewFrom, a.mode, foldBefore, fromUser, viewUpdated)
    // the queued updates is NOT applied in this method, instead they are applied after any flush!!!
    if (updatingState) throw new IllegalStateException("You should not update state during a state update!!!")
    if (debug_view) {
      //println("client update inner root: " + res.root)
//      println("client update view transactions: " + viewFrom)
//      println("client update mode: " + a.mode)
    }
    updatingState = true
    val modeBefore = state.mode.getOrElse(modeTemp)
    val docBefore = state.node
    state_ = a
    if (scheduledUpdateTempMode != null) {
      scheduledUpdateTempMode.cancel()
    }
    if (modeTemp != null) {
      scheduledUpdateTempMode = Observable.delay({
        println("updating temp mode")
        updateState(a.copy(mode = Some(modeTemp)), null, Seq.empty, Undoer.Local, false, Seq.empty, false)
      }).delaySubscription(2.seconds).subscribe()
    }
    this.modeTemp = modeTemp
    if (foldBefore.nonEmpty) {
      localStorage.set(docId + ".folded", state_.foldedNodes.mkString(","))
    }
    onBeforeUpdateUpdateCommandState(state_)
    trackUndoerChange(from, ty, modeBefore, docBefore)
    stateUpdates_.onNext(res)
    updatingState = false
    if (state_.isRichInsert) {
      if (insertingFlusher == null) {
        insertingFlusher = Observable.interval(100.millis).doOnNext(_ => flush()).subscribe()
      }
    } else {
      if (insertingFlusher != null) {
        insertingFlusher.cancel()
        insertingFlusher = null
      }
    }
  }
  def state: DocState = state_

  /**
    * request queue
    */
  private var requesting = false
  private var requests: Seq[Int] = Seq.empty[Int]

  private def putBackAndMarkNotConnected(head: Int): Unit = {
    requests = head +: requests
    connection_.update(None)
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
          connection_.update(None)
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
    if (disableUpdateBecauseLocalNodeDelete != null) {
      if (System.currentTimeMillis() - disableUpdateBecauseLocalNodeDelete._2 > 5000) {
        disableUpdateBecauseLocalNodeDelete = null
      }
    }
    if (disableUpdateBecauseLocalNodeDelete != null) {
      return
    }
    if (!requesting) {
      requests match {
        case head :: tail =>
          requests = tail
          val submit = uncommitted
          request[ClientUpdate](head, server.change(authentication, committedVersion, submit, state.mode, if (debug_model) committed else data.Node.debug_empty).call(), succsss => {
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
    if (disableStateUpdate) {
      disabledStateUpdates.append(success)
    } else {
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
        connection_.update(Some(success.serverStatus))
        if (wp0.nonEmpty) {
          val nn = operation.Node.apply(wp0, state.node)
          val mmm = operation.Node.transform(nn, wp0, state.mode.map(a => (a, false)).getOrElse((modeTemp, true)))
          if (debug_view) {
            println("after sever mode is: " + mmm)
          }
          updateState(
            state.copy(
              node = nn,
              mode = if (mmm._2) None else Some(mmm._1)
            ),
            if (mmm._2) mmm._1 else null,
            wp0,
            Undoer.Remote,
            viewUpdated = false,
            wp0,
            fromUser = false)
      }
      } catch {
        case e: Exception =>
          throw new Exception(s"Apply update from server failed $success #### $committed", e)
      }
    }
  }


  override def exitCodeEditMode(ps: Seq[model.operation.Unicode]): Unit = {
    if (state.isCodeInside) {
      val at = state.asCodeInside
      localChange(DocTransaction(
        ps.map(o => model.operation.Node.Content(at, model.operation.Content.CodeContent(o)))
        , Some(state.copyContentMode(mode.Content.CodeNormal))))

    }
  }

  /**
    * view calls this method to insert text at current insertion point,
    */
  def onInsertRichTextAndViewUpdated(unicode: Unicode): Unit = {
    import model._
    val (n, _, insert) = state.asRichInsert
    localChange(
      DocTransaction(Seq(operation.Node.rich(n, operation.Rich.insert(insert.pos, unicode))),
      Some(state.copyContentMode(mode.Content.RichInsert(insert.pos + unicode.size))),
      viewUpdated = true))
  }

  override def onAttributeModified(cur: Node, range: IntRange, url: data.Unicode, title: data.Unicode): Unit = {
    val rich = state.node(cur).rich
    localChange(DocTransaction(Seq(
      operation.Node.rich(cur, operation.Rich.changeAttributeAt(rich, range, url, title))
    ), state.mode))
  }

  override def onExternalPastePlain(unicode: Unicode): Unit = {
    val update = state.mode match {
      case Some(m) => m match {
        case model.mode.Node.Content(n, c) =>
          def insert(a: Int): operation.Node = operation.Node.rich(n, operation.Rich.insert(a, unicode))
          c match {
            case model.mode.Content.RichNormal(r) =>
              DocTransaction(Seq(insert(r.start)),
                Some(state.copyContentMode(mode.Content.RichNormal(r.moveBy(unicode.size)))))
            case model.mode.Content.RichInsert(pos) =>
              DocTransaction(Seq(insert(pos)),
                Some(state.copyContentMode(mode.Content.RichInsert(pos + unicode.size))))
            case model.mode.Content.RichVisual(f, m) =>
              val rg = f.merge(m)
              val rich = state.rich(n)
              val op = operation.Rich.insert(rg.start, unicode)
              val applied = op(rich)
              operation.Rich.deleteTextualRange(applied, rg.moveBy(unicode.size)) match {
                case Some((a, b, _)) =>
                  DocTransaction(operation.Node.rich(n, op) +: a.map(o => operation.Node.rich(n, o)),
                    Some(state.copyContentMode(model.mode.Content.RichNormal(b))))
                case None =>
                  DocTransaction(Seq(operation.Node.rich(n, op)),
                    Some(state.copyContentMode(mode.Content.RichNormal(f.min(m).moveBy(unicode.size)))))
              }
            case model.mode.Content.CodeNormal =>
              DocTransaction.empty
              // LATER paste in code normal
            case model.mode.Content.CodeInside =>
              DocTransaction.empty
              // LATER  paste in code inside
          }
        case model.mode.Node.Visual(fix, move) =>
          DocTransaction.empty
      }
      case _ => DocTransaction.empty
    }
    localChange(update)
  }

  def applyFolds(folded0: Set[String], unfold0: Set[cursor.Node], toggle0: Set[cursor.Node], changes: transaction.Node): (Set[String], Map[cursor.Node, Boolean]) = {
    if (unfold0.isEmpty && toggle0.isEmpty) {
      (folded0, Map.empty)
    } else {
      val toggle = toggle0.map(c => (c, state.node(c).uuid))
      val unfold = unfold0.map(c => (c, state.node(c).uuid))
      var toFold = toggle.filter(a => !folded0.contains(a._2))
      val toUnfold = (toggle -- toFold) ++ unfold.filter(a => folded0.contains(a._2))
      toFold = toFold -- toUnfold
      (folded0 ++ toFold.map(_._2) -- toUnfold.map(_._2), toFold.map(a => a._1 -> true).toMap ++ toUnfold.map(a => a._1 -> false).toMap)
    }
  }

  def localChange(update0: DocTransaction): Unit = {
    var update: DocTransaction = update0
    if (disableStateUpdate) throw new IllegalStateException("You have disabled state update!!!")
    // for a delete of a non empty node, we disable sync from server for sometime
    // during this time, if the next local change is an insert, we try to merge them as a move
    var viewAdd : transaction.Node = Seq.empty
    var justDisabled = false
    if (update.tryMergeDeletes) {
      update.transaction match {
        case Seq(d@operation.Node.Delete(r)) =>
          val nodes = state.node(r)
          if (nodes.exists(a => a.content.nonEmpty || a.childs.nonEmpty)) {
            disableUpdateBecauseLocalNodeDelete = (d, System.currentTimeMillis(), nodes, state)
            justDisabled = true
          }
      }
    }
    if (!justDisabled) {
      if (disableUpdateBecauseLocalNodeDelete != null &&
        uncommitted.lastOption.contains(Seq(disableUpdateBecauseLocalNodeDelete._1))) {
        val d = disableUpdateBecauseLocalNodeDelete._1
        if (update.tryMergeInsertOfDeleteRange.contains(d.r)) {
          update.transaction match {
            case Seq(i@operation.Node.Insert(at, childs)) if childs == disableUpdateBecauseLocalNodeDelete._3 =>
              cutOneLocalHistory(Seq(d))
              state_ = disableUpdateBecauseLocalNodeDelete._4.copy(foldedNodes = state_.foldedNodes)
              val inverse = d.reverse(state.node)
              uncommitted = uncommitted.dropRight(1)
              viewAdd = Seq(inverse)
              val before = d.r.transformBeforeDeleted(at)
              update = update.copy(transaction = Seq(operation.Node.Move(d.r, before)))
          }
        }
      }
      if (update.transaction.nonEmpty) {
        disableUpdateBecauseLocalNodeDelete = null
      }
    }
    val changes = update.transaction
    val mode = update.mode
    val d = if (changes.nonEmpty) {
      if (update.handyAppliedResult.isDefined) {
        update.handyAppliedResult.get
      } else {
        operation.Node.apply(changes, state.node)
      }
    } else {
      state.node
    }
    val m = mode match {
      case Some(k) =>
        k
      case None =>
        val mmm = state.mode.getOrElse(modeTemp)
        operation.Node.transform(d, changes, (mmm, false))._1
    }
    for (m <- update.viewMessagesBefore) {
      viewMessages_.onNext(m)
    }
    if (!update.nonTransactional) {
      if (debug_view) {
        println(update)
      }
      val (res, ch) = applyFolds(state.foldedNodes, update.unfoldBefore, update.toggleBefore, changes)
      updateState(DocState(d, Some(m), res),
        null,
        changes,
        update.undoType.getOrElse(Undoer.Local),
        viewUpdated = update.viewUpdated,
        if (viewAdd.isEmpty) changes else viewAdd ++ changes,
        fromUser = true,
        foldBefore = ch)
      if (changes.nonEmpty) uncommitted = uncommitted :+ changes
      self.sync()
    }
  }

  override def onKeyDown(k: Key): Boolean = keyDown(k)

}
