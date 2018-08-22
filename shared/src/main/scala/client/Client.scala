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
import model.data.{CodeType, SpecialChar, Unicode}
import command._
import doc.{DocInterface, DocState, DocTransaction, DocUpdate}
import model.cursor.Node
import model.mode.Content.CodeInside
import model.range.IntRange
import monix.reactive.subjects.PublishSubject
import register.RegisterHandler
import undoer.Undoer
import view.EditorInterface

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.Future
import scala.util.{Failure, Random, Success, Try}


object Client {
  type Proxy = ClientProxy[Api, ByteBuffer, boopickle.Pickler, boopickle.Pickler]



  sealed trait ViewMessage {
  }
  object ViewMessage {
    case class VisitUrl(url: String) extends ViewMessage
    case class ShowCommandMenu() extends ViewMessage
    case object ScrollToTop extends ViewMessage
    case object ScrollToBottom extends ViewMessage
    case class QuickSearch(currentDoc: Boolean) extends ViewMessage
    case class CopyToClipboard(a: String) extends ViewMessage
    case class SimulateKeyboardMotion(isUp: Boolean) extends ViewMessage
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
  with InputRuler
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
  private var disableUpdateBecauseLocalNodeDelete: (operation.Node.Delete, Long, Seq[data.Node], DocState) = null

  
  /**
    * document observable
    *
    * editor queue
    */
  private var state_ = {
    val zoom = localStorage.get(docId + ".zoom1") match {
      case Some(uuid) =>
        committed.lookup(uuid).getOrElse(model.cursor.Node.root)
      case _ => cursor.Node.root
    }
    DocState(committed,
      zoom,
      model.mode.Node.Content(zoom, committed(zoom).content.defaultNormalMode()),
      badMode = false, localStorage.get(docId + ".folded0") match {
      case Some(s) => s.split(",").filter(_.nonEmpty).map(a => {
        val c = a.split(" ")
        c(0) -> c(1).toBoolean
      }).toMap
      case _ => Map.empty
    })
  }

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
    from: Seq[(DocState, operation.Node)],
    viewAdded: Seq[(DocState, operation.Node)],
    ty: Undoer.Type,
    viewUpdated: Boolean = false,
    editorUpdated: Boolean = false,
    fromUser: Boolean = false,
    isSmartInsert: Boolean = false,
    userFolds: Map[cursor.Node, Boolean] = Map.empty
  ): Unit = {
    val vv = viewAdded ++ from
    val res = DocUpdate(a,
      if (vv.isEmpty) Seq.empty else vv.zip(vv.tail.map(_._1) :+ a).map(a => (a._1._1, a._1._2, a._2)),
      userFolds, fromUser, viewUpdated, editorUpdated)
    // the queued updates is NOT applied in this method, instead they are applied after any flush!!!
    if (updatingState) throw new IllegalStateException("You should not update state during a state update!!!")
    if (debug_view) {
      //println("client update inner root: " + res.root)
//      println("client update view transactions: " + viewFrom)
//      println("client update mode: " + a.mode)
    }
    val zoomPrev = state.zoomId
    updatingState = true
    val docBefore = state
    state_ = a
    if (scheduledUpdateTempMode != null) {
      scheduledUpdateTempMode.cancel()
    }
    if (a.badMode) {
      scheduledUpdateTempMode = Observable.delay({
        if (model.debug_view) println("updating temp mode")
        lockObject.synchronized {
          updateState(state_.copy(badMode = false), Seq.empty, Seq.empty, Undoer.Local)
        }
      }).delaySubscription(2.seconds).subscribe()
    }
    if (userFolds.nonEmpty) {
      localStorage.set(docId + ".folded0", state_.userFoldedNodes.toSeq.map(a => s"${a._1} ${a._2}").mkString(","))
    }
    val zoomNow = state.zoomId
    if (zoomPrev != zoomNow) {
      localStorage.set(docId + ".zoom1", zoomNow)
    }

    onBeforeUpdateUpdateCommandState(state_)
    trackUndoerChange(docBefore, state_, from.map(_._2), ty, isSmartInsert)
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

  private var lastRequestTime = 0L

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
          if (submit.nonEmpty || System.currentTimeMillis() - lastRequestTime >= 1000) {
            lastRequestTime = System.currentTimeMillis()
            request[ClientUpdate](head, server.change(authentication, committedVersion, submit, state.mode, if (debug_model) committed else data.Node.debug_empty).call(), succsss => {
              updateFromServer(succsss)
            })
          }
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
        val flatten = operation.Node.merge(winners.flatten, false)
        val (loser, remaining) = uncommitted.splitAt(take)
        // LATER handle conflict, modal handling of winner deletes loser
        val Rebased(cs0, (wp, lp)) = ot.Node.rebaseT(flatten, loser)
        committed = operation.Node.applyT(lp, operation.Node.apply(flatten, committed))
        val altVersion = committedVersion + winners.size + lp.size
        committedVersion = success.finalVersion
        assert(altVersion == committedVersion, s"Version wrong! $committedVersion $altVersion ${winners.size} $take")
        val Rebased(cs1, (wp0, uc)) = ot.Node.rebaseT(wp, remaining)
        uncommitted = uc
        connection_.update(Some(success.serverStatus))
        if (wp0.nonEmpty) {
          val (last, from) = operation.Node.apply(wp0, state)
          updateState(
            last,
            from,
            Seq.empty,
            Undoer.Remote)
      }
      } catch {
        case e: Exception =>
          throw new Exception(s"Apply update from server failed $success #### $committed", e)
      }
    }
  }


  override def exitSubMode(): Unit = {
    sourceEditorCommandBuffer.update("")
    state.mode match {
      case Some(model.mode.Node.Content(cur, sub: model.mode.Content.RichSubMode)) =>
        localChange(DocTransaction(state.copyContentMode(sub.modeBefore)))
      case Some(model.mode.Node.Content(cur, model.mode.Content.CodeInside(mode, pos))) =>
        localChange(DocTransaction(state.copyContentMode(model.mode.Content.CodeNormal)))
      case _ =>
    }
  }


  override def onCodeTypeChangeAndEditorUpdated(to: CodeType): Unit = {
    state.mode match {
      case Some(model.mode.Node.Content(cur, model.mode.Content.RichCodeSubMode(range, code, modeBefore))) =>
        val aft = state.rich(cur).after(range.start - 1)
        assert(aft.text.asDelimited.delimitation.newDeliEndSize == 1)
        val deli = aft.text.asDelimited
        localChange(DocTransaction(Seq(
          operation.Node.rich(cur, operation.Rich.wrapUnwrap(aft.textRange.start, deli, to.delimitation))
        ), state.mode,
          editorUpdated = true))
      case Some(model.mode.Node.Content(cur, model.mode.Content.CodeInside(mode, pos))) =>
        localChange(DocTransaction(
          Seq(model.operation.Node.Content(cur, model.operation.Content.CodeLang(to.str)))
          , None, editorUpdated = true))
      case _ => throw new IllegalStateException("What??")
    }
  }

  override def onChangeAndEditorUpdated(op: Seq[operation.Unicode], inside: CodeInside): Unit = {
    state.mode match {
      case Some(model.mode.Node.Content(cur, model.mode.Content.RichCodeSubMode(range, code, modeBefore))) =>
        val rich = state.rich(cur)
        val aft = rich.after(range.start - 1)
        assert(aft.text.asDelimited.delimitation.newDeliEndSize == 1)
        val trans = if (op.nonEmpty) {
          val ot = operation.Rich.fromCode(range.start, op)
          Seq(operation.Node.rich(cur, ot))
        } else {
          Seq.empty
        }
        if (trans.isEmpty && inside == code) {
          // do nothing
        } else {
          localChange(DocTransaction(trans, None, subCodeMode = Some(inside), editorUpdated = true))
        }
      case Some(model.mode.Node.Content(cur, c: model.mode.Content.CodeInside)) =>
        if (op.isEmpty && inside == c) {
          // do nothing
        } else {
          localChange(DocTransaction(
            op.map(o => model.operation.Node.Content(cur, model.operation.Content.CodeContent(o)))
            , Some(state.copyContentMode(inside)), editorUpdated = true))
        }
      case _ => throw new IllegalStateException("What??")
    }
  }

  override def refreshMode(): Unit = {
    updateState(state, Seq.empty, Seq.empty, Undoer.Local)
  }

  override def focusOn(c: Node, ran: Option[IntRange], viewUpdated: Boolean): Boolean = {
    import model._
    state.mode match {
      case Some(mode.Node.Content(cur, rich: mode.Content.Rich)) =>
        if (cur == c && ran.isEmpty) {
          return false
        }
      case _ =>
    }
    val mo = state.node(c).content match {
      case c@data.Content.Rich(r) =>
        ran match {
          case Some(a) =>
            if (a.isEmpty) {
              if (state.isInsert) {
                mode.Content.RichInsert(a.start)
              } else {
                mode.Content.RichNormal(r.rangeAfter(a.start))
              }
            } else {
              val r1 = r.rangeAfter(a.start)
              val r2 = r.rangeBefore(a.until)
              if (r1 == r2) {
                mode.Content.RichNormal(r1)
              } else {
                mode.Content.RichVisual(r1, r2)
              }
            }
          case None =>
            c.defaultNormalMode()
        }
      case c: data.Content.Code => c.defaultNormalMode()
    }
    // don't update the view
    localChange(DocTransaction(Seq.empty, Some(model.mode.Node.Content(c, mo))))
    true
  }


  /**
    * view calls this method to insert text at current insertion point,
    */

  override def onInsertRichTextAndViewUpdated(start: Int, end: Int, unicode: Unicode, domInsertion: Int): Unit = {
    import model._
    state.mode match {
      case Some(mode.Node.Content(cur, rich: mode.Content.Rich)) =>
        val before = state.node(cur).rich
        val afterSize = before.size + (end - start) + unicode.size
        val m = if (rich.isInstanceOf[mode.Content.RichInsert]) {
          if (domInsertion >= 0 && domInsertion <= afterSize) {
            mode.Content.RichInsert(domInsertion)
          } else {
            mode.Content.RichInsert(start + unicode.size)
          }
        } else {
          def rangeOf(tuple: (Int, Unicode)) = {
            IntRange.len(tuple._1, tuple._2.size)
          }
          val range = if (domInsertion >= 0) {
            if (domInsertion <= start) {
              if (domInsertion == 0 ) {
                if (unicode.isEmpty) {
                  before.rangeAfter(end)
                } else {
                  rangeOf(unicode.after(0).next()).moveBy(start)
                }
              } else {
                before.rangeBefore(domInsertion)
              }
            } else if (domInsertion > start && domInsertion <= start + unicode.size) {
              rangeOf(unicode.before(domInsertion - start).next()).moveBy(start)
            } else {
              before.rangeBefore(domInsertion - unicode.size + (end - start)).moveBy(unicode.size - (end - start))
            }
          } else {
            if (unicode.isEmpty) {
              if (start == 0) {
                before.rangeAfter(end).moveBy(-end)
              } else {
                before.rangeBefore(start)
              }
            } else {
              rangeOf(unicode.before(unicode.size).next()).moveBy(start)
            }
          }
          mode.Content.RichNormal(range)
        }
        localChange(
          DocTransaction(Seq(operation.Node.rich(cur, operation.Rich.replacePlain(start, end, unicode))),
            Some(state.copyContentMode(m)),
            viewUpdated = true))
      case _ => throw new IllegalStateException("Not supported")
    }

  }


  val sourceEditorCommandBuffer = ObservableProperty("")
  def sourceEditorCommands: Observable[String] = sourceEditorCommandBuffer

  def onSourceEditorCommandBuffer(a: String): Unit = {
    sourceEditorCommandBuffer.update(a)
  }


  override def onSourceEditorUndo(): Unit = {
    localChange(undo(state))
  }

  override def onSourceEditorRedo(): Unit = {
    localChange(redo(state))
  }

  override def onAttributeModified(url: data.Unicode, title: data.Unicode): Unit = {
    state.mode match {
      case Some(model.mode.Node.Content(node, model.mode.Content.RichAttributeSubMode(range, modeBefore))) =>
        val rich = state.node(node).rich
        localChange(DocTransaction(Seq(
          operation.Node.rich(node, operation.Rich.changeAttributeAt(rich, range.start - 1, url, title))
        ), None))
      case _ => throw new IllegalStateException("What??")
    }
  }



  /**
    * currently code editors system copy/paste is not handled by this
    */
  override def onExternalPasteInRichEditor(unicode: Unicode): Unit = {
    state.mode.map {
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
            // LATER paste in code normal
            DocTransaction.empty
          case _ =>
            throw new IllegalStateException("not handled by me")
        }
      case model.mode.Node.Visual(fix, move) =>
        DocTransaction.empty
    }.foreach(t => localChange(t))
  }

  def applyFolds(folded0: DocState,
    unfold0: Set[cursor.Node],
    toggle0: Set[cursor.Node]
  ): (Map[String, Boolean], Map[cursor.Node, Boolean]) = {
    if (unfold0.isEmpty && toggle0.isEmpty) {
      (folded0.userFoldedNodes, Map.empty)
    } else {
      val toggle = toggle0.map(c => (c, state.node(c)))
      val unfold = unfold0.map(c => (c, state.node(c)))
      var toFold = toggle.filter(a => !folded0.folded(a._1))
      val toUnfold = (toggle -- toFold) ++ unfold.filter(a => folded0.folded(a._1))
      toFold = toFold -- toUnfold
      (folded0.userFoldedNodes
        ++ toFold.filter(!_._2.isH1).map(_._2.uuid -> true).toMap
        ++ toUnfold.filter(_._2.isH1).map(_._2.uuid -> false).toMap
        -- toFold.filter(_._2.isH1).map(_._2.uuid)
        -- toUnfold.filter(!_._2.isH1).map(_._2.uuid)
        , toFold.map(a => a._1 -> true).toMap ++ toUnfold.map(a => a._1 -> false).toMap)
    }
  }

  def localChange(update0: DocTransaction, isSmartInsert: Boolean = false): Unit = this.synchronized {
    var update: DocTransaction = update0
    if (debug_view) {
      println(update)
    }
    if (disableStateUpdate) throw new IllegalStateException("You have disabled state update!!!")
    // for a delete of a non empty node, we disable sync from server for sometime
    // during this time, if the next local change is an insert, we try to merge them as a move
    var viewAdd : Seq[(DocState, operation.Node)] = Seq.empty
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
              val os = state_
              val zz = d.r.transformBeforeDeleted(os.zoom)
              val td = disableUpdateBecauseLocalNodeDelete._4
              state_ = td.copy(userFoldedNodes = state_.userFoldedNodes, zoom = zz, mode0 = model.mode.Node.Content(zz, td.node(zz).content.defaultNormalMode()))
              uncommitted = uncommitted.dropRight(1)
              val inverse = d.reverse(state.node)
              viewAdd = Seq((os, inverse))
              update = update.copy(transaction = Seq(operation.Node.Move(d.r, d.r.transformBeforeDeleted(at))))
          }
        }
      }
      if (update.transaction.nonEmpty) {
        disableUpdateBecauseLocalNodeDelete = null
      }
    }
    val extra = update.extra match {
      case e@Some(_) => e
      case _ if !isSmartInsert => extraInputRuleOperation(state_, update.transaction)
      case _ => None
    }
    val (last0, from) = operation.Node.apply(update.transaction, state)
    val (res, ch) = applyFolds(state, update.unfoldBefore, update.toggleBefore)
    val toMode: model.mode.Node = update.mode.getOrElse(last0.mode0 match {
      case c@model.mode.Node.Content(cur, sub: model.mode.Content.RichCodeSubMode) =>
        update.subCodeMode match {
          case Some(scm) =>
            model.mode.Node.Content(cur, sub.copy(code = scm))
          case None => c
        }
      case a => a
    })
    val last = last0.copy(mode0 = toMode,
      badMode = false,
      zoom = update.zoomAfter.getOrElse(last0.zoom),
      userFoldedNodes = res)
    for (m <- update.viewMessagesBefore) {
      viewMessages_.onNext(m)
    }
    if (!update.nonTransactional) {
      updateState(last,
        from,
        viewAdd,
        update.undoType.getOrElse(Undoer.Local),
        isSmartInsert = isSmartInsert,
        viewUpdated = update.viewUpdated,
        editorUpdated = update.editorUpdated,
        fromUser = true,
        userFolds = ch)
      if (update.transaction.nonEmpty) uncommitted = uncommitted :+ update.transaction
      self.sync()
    }
    for (m <- update.viewMessagesAfter) {
      viewMessages_.onNext(m)
    }
    extra match {
      case Some(a) => localChange(a, isSmartInsert = true)
      case None =>
    }
  }

  override def onKeyDown(k: Key): Boolean = keyDown(k)

}
