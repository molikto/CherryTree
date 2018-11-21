package client


import java.io.Closeable
import java.nio.ByteBuffer
import java.util.UUID

import com.softwaremill.quicklens._
import monix.execution.Cancelable
import monix.reactive.Observable
import monix.execution.rstreams.Subscription
import monix.reactive._

import concurrent.duration._
import monix.execution.Scheduler.Implicits.global
import command.Key.{Delete, KeySeq}
import model.ot.Rebased
import util._
import model._
import api.{unpickleState, _}
import boopickle.{PickleState, Pickler}
import model.data.{CodeType, SpecialChar, Text, Unicode}
import command._
import doc.{DocInterface, DocState, DocTransaction, DocUpdate}
import io.lemonlabs.uri.{AbsoluteUrl, Url}
import model.cursor.Node
import model.mode.Content.CodeInside
import model.operation.Rich
import model.range.IntRange
import monix.reactive.subjects.PublishSubject
import register.{RegisterHandler, Registerable}
import search.{SearchHandler, SearchState}
import settings.{Settings, SettingsImpl}
import undoer.Undoer
import view.EditorInterface

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.Future
import scala.util.{Failure, Random, Success, Try}


trait Api {
  val localStorage: LocalStorage

  def get[S](path: String)
    (implicit s: Pickler[S], unpickleState: ByteBuffer => model.UnpickleState): Future[S] = {
    val bytes = ByteBuffer.wrap(Array.empty)
    requestBytes(path, bytes, "GET").map(b => Unpickle[S](implicitly).fromBytes(b)(unpickleState))
  }

  def request[S](path: String)
    (implicit s: Pickler[S], unpickleState: ByteBuffer => model.UnpickleState): Future[S] = {
    val bytes = ByteBuffer.wrap(Array.empty)
    requestBytes(path, bytes, "POST").map(b => Unpickle[S](implicitly).fromBytes(b)(unpickleState))
  }

  def request[R, S](path: String, content: R)
    (implicit pickleState: PickleState, r: Pickler[R], s: Pickler[S], unpickleState: ByteBuffer => model.UnpickleState): Future[S] = {
    val bytes = Pickle.intoBytes(content)(implicitly, implicitly)
    requestBytes(path, bytes, "POST").map(b => Unpickle[S](implicitly).fromBytes(b)(unpickleState))
  }

  def requestBytes(path: String, content: ByteBuffer = ByteBuffer.wrap(Array.empty[Byte]), method: String): Future[ByteBuffer]

  def setupWebSocket(path: String): (Closeable, Observable[Either[String, Throwable]])
}

object Client {

  sealed trait ViewMessage {
  }
  object ViewMessage {
    case class VisitUrl(url: String) extends ViewMessage
    case class ShowCommandMenu() extends ViewMessage
    case class ShowRegisters() extends ViewMessage
    case object ScrollToTop extends ViewMessage
    case object ScrollToBottom extends ViewMessage
    case class ScrollToNodeTop(cur: cursor.Node) extends ViewMessage
    case class QuickSearch(currentDoc: Boolean) extends ViewMessage
    case class CopyToClipboard(a: String) extends ViewMessage
    case class VisualUpDownMotion(isUp: Boolean, blockWiseCount: Int, intoVisual: Boolean) extends ViewMessage
    case object ExitVisual extends ViewMessage
  }
}

class Client(
  val docId: UUID,
  initial: InitResponse,
  private val api: Api,
) extends SettingsImpl
  with RegisterHandler
  with Undoer
  with EditorInterface
  with DocInterface { self =>

  var _commandHandler: CommandHandler = new CommandHandler(this)
  def commands = _commandHandler

  private var inputRuler = new InputRuler()
  val searchHandler = new SearchHandler(this)


  def initInputRulerAndCommandHandler(): Unit = {
    _commandHandler.commands.flatMap(_.inputRule).foreach(inputRuler.registerInputRule)
  }

  initInputRulerAndCommandHandler()

  def changeSettings(temp: Settings): Boolean = {
    var res = false
    if (enableModal != temp.enableModal) {
      enableModal = temp.enableModal
      state_ = state_.copy(mode0 = model.mode.Node.Content(state_.zoom, state_.node(state_.zoom).content.defaultMode(enableModal)))
      _commandHandler = new CommandHandler(this)
      inputRuler = new InputRuler()
      writeEnableModal()
      res = true
    }
    res
  }


  override def localStorage: LocalStorage = api.localStorage

  def debug_unmarkTempDisableMode() = {
    state_ = state_.copy(badMode = false)
  }


  protected def lockObject: AnyRef  = self

  /**
    * connection state
    */
  private val connection_ = ObservableProperty[ConnectionStatus](ConnectionStatus(initial.serverStatus))

  def connection: Observable[ConnectionStatus] = connection_

  private val errors_ : ObservableProperty[Option[Throwable]] = ObservableProperty(None)

  def showError(err: Option[Throwable]) = errors_.update(err)

  /**
    * last error
    */
  def errors: Observable[Option[Throwable]] = errors_

  protected val viewMessages_ : PublishSubject[Client.ViewMessage] = PublishSubject()

  def viewMessages: Observable[Client.ViewMessage] = viewMessages_



  private var subscription: Cancelable = null

  def start(): Unit = this.synchronized {
    val (ws, obs) = api.setupWebSocket(s"/document/$docId/ws")
    subscription = obs.doOnNext {
      case Left(str) => sync()
      case Right(ex) =>
        // we mark it as offline, then try to sync again
        connection_.modify(_.copy(offline = true))
    }.doAfterTerminate(_ => ws.close()).subscribe()
  }

  private var stopped = false

  def destory(): Unit = this.synchronized {
    stopped = true
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
  private var uncommittedIds = Seq.empty[UUID]
  def version: Int = committedVersion + uncommitted.size
  private var disableUpdateBecauseLocalNodeDelete: (operation.Node.Delete, Long, Seq[data.Node], DocState) = null


  /**
    * document observable
    *
    * editor queue
    */
  private var state_ = {
    val zoom = api.localStorage.get(docId + ".zoom1") match {
      case Some(uuid) =>
        committed.lookup(UUID.fromString(uuid)).getOrElse(model.cursor.Node.root)
      case _ => cursor.Node.root
    }
    DocState(committed,
      zoom,
      model.mode.Node.Content(zoom, committed(zoom).content.defaultMode(enableModal)),
      badMode = false, api.localStorage.get(docId + ".folded0") match {
      case Some(s) => s.split(",").filter(_.nonEmpty).map(a => {
        val c = a.split(" ")
        UUID.fromString(c(0)) -> c(1).toBoolean
      }).toMap
      case _ => Map.empty
    })
  }

  private val flushes_ : PublishSubject[Unit] = PublishSubject[Unit]()


  private var tempDisables: Set[String] = Set.empty[String]
  private var flushing = false
  private var updatingState = false
  private def tempDisableRemoteStateUpdate: Boolean = tempDisables.nonEmpty

  private def updateConnectionStatusBasedOnDisableStatus(): Unit = {
    val before = connection_.get
    val offlineOfDelete = disableUpdateBecauseLocalNodeDelete != null
    if (before.offline != offlineOfDelete || before.tempOffline != tempDisables) {
      connection_.modify(_.copy(tempOffline = tempDisables, nodeDeletePending = offlineOfDelete))
    }
  }

  def disableRemoteStateUpdate(disable: Boolean, reason: String): Unit = this.synchronized {
    if (flushing) throw new IllegalStateException("You should not change state will fetching state!!")
    if (disable) {
      tempDisables = tempDisables + reason
    } else {
      tempDisables = tempDisables - reason
    }
    flushInner()
    updateConnectionStatusBasedOnDisableStatus()
  }

  def flushes: Observable[Unit] = flushes_

  private def flushInner(): Unit = {
    if (!tempDisableRemoteStateUpdate) {
      flushing = true
      // send flush event first to have consistent local state
      flushes_.onNext(Unit)
      flushing = false
      disabledStateUpdates.foreach(a => {
        updateFromServer(a)
      })
      disabledStateUpdates.clear()
    }
  }



  private val disabledStateUpdates = ArrayBuffer[ChangeResponse]()

  private val stateUpdates_ : PublishSubject[DocUpdate] = PublishSubject[DocUpdate]()

  var preStateUpdate: Option[DocUpdate => Unit] = None

  def stateUpdates: Observable[DocUpdate] = stateUpdates_

  //private var insertingFlusher: Cancelable = null

  private var scheduledUpdateTempMode: Cancelable = null

  private def updateState(a: DocState,
    from: Seq[(DocState, operation.Node)],
    viewAdded: Seq[(DocState, operation.Node)],
    ty: Undoer.Type,
    trace: Throwable = null,
    viewUpdated: Boolean = false,
    editorUpdated: Boolean = false,
    fromUser: Boolean = false,
    isSmartInsert: Boolean = false,
    mergeWithPreviousLocal: Boolean = false,
    userFolds: Map[cursor.Node, Boolean] = Map.empty
  ): Unit = {
    a.consistencyCheck(enableModal)
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
          updateState(state_.copy(badMode = false), Seq.empty, Seq.empty, Undoer.Local, trace = trace)
        }
      }).delaySubscription(2.seconds).subscribe()
    }
    if (userFolds.nonEmpty) {
      api.localStorage.set(docId + ".folded0", state_.userFoldedNodes.toSeq.map(a => s"${a._1} ${a._2}").mkString(","))
    }
    val zoomNow = state.zoomId
    if (zoomPrev != zoomNow) {
      api.localStorage.set(docId + ".zoom1", zoomNow.toString)
    }


    _commandHandler.onBeforeUpdateUpdateCommandState(state_)
    trackUndoerChange(docBefore, state_, from.map(_._2), ty, isSmartInsert, mergeWithPreviousLocal)
    preStateUpdate.foreach(_(res))
    stateUpdates_.onNext(res)
    updatingState = false
//    if (state_.isRichInsert) {
//      if (insertingFlusher == null) {
//        insertingFlusher = Observable.interval(100.millis).doOnNext(_ => flush()).subscribe()
//      }
//    } else {
//      if (insertingFlusher != null) {
//        insertingFlusher.cancel()
//        insertingFlusher = null
//      }
//    }

    if (model.debug_model) {
      if (enableModal || !state_.copy(badMode = false).isRichNormal) {

      } else {
        if (trace != null) {
          trace.printStackTrace()
        }
        assert(false, s"you should not in modal mode now! ${state_.mode0}")
      }
      state_.mode0 match {
        case model.mode.Node.Content(cur, r) =>
          if (state_.node(cur).content.isRich) {
            if (!state_.copy(badMode = false).isRich) {
              if (trace != null) {
                trace.printStackTrace()
              }
              assert(state_.isRich, s"inconsistent state ${state_.mode0} ${state_.badMode} ${vv.map(_._2)}")
            }
          } else {
            if (!state_.copy(badMode = false).isCode) {
              if (trace != null) {
                trace.printStackTrace()
              }
              assert(state_.isCode, s"inconsistent state ${state_.mode0} ${state_.badMode} ${vv.map(_._2)}")
            }
          }
        case _ =>
      }
    }
  }
  def state: DocState = state_

  /**
    * request queue
    */
  private var lastSyncRequest = 0l
  private var requesting = false

  private def putBackAndMarkNotConnected(head: Int): Unit = {
    connection_.modify(_.copy(offline = true))
  }


  override def getNodeInfo(uuid: UUID): Future[Option[NodeInfo]] = {
    val request = api.get[Option[NodeInfo]](s"/document/$docId/node/$uuid/info")
    request.failed.foreach {
      case e => e.printStackTrace()
    }
    request
  }

  private def request[T](head: Int, a: Future[T], onSuccess: T => Unit): Unit = {
    requesting = true
    a.onComplete {
      case Success(r) =>
        self.synchronized {
          requesting = false
          onSuccess(r)
          tryTopRequest()
        }
      case Failure(t) =>
        println(t.getMessage)
        self.synchronized {
          requesting = false
          connection_.modify(_.copy(offline = true))
          t match {
            case _ =>
              // LATER properly handle this!
              putBackAndMarkNotConnected(head)
          }
        }
    }
  }

  private var lastRequestTime = 0L

  private def updateDisableUpdateBecauseLocalNodeDelete(): Boolean = {
    if (disableUpdateBecauseLocalNodeDelete != null) {
      if (System.currentTimeMillis() - disableUpdateBecauseLocalNodeDelete._2 > 5000) {
        disableUpdateBecauseLocalNodeDelete = null
        markAllAsNeedsClone()
      }
    }
    disableUpdateBecauseLocalNodeDelete != null
  }

  def debug_blockReason = s"${disableUpdateBecauseLocalNodeDelete != null} $requesting $tempDisableRemoteStateUpdate"

  private def tryTopRequest(): Boolean = {
    if (!updateDisableUpdateBecauseLocalNodeDelete() && !requesting && !tempDisableRemoteStateUpdate) {
      val submit = uncommitted
      while (uncommittedIds.size < uncommitted.size) {
        uncommittedIds = uncommittedIds :+ UUID.randomUUID()
      }
      val submitIds = uncommittedIds
      if (submit.nonEmpty || System.currentTimeMillis() - lastRequestTime >= 1000 || lastSyncRequest > lastRequestTime) {
        lastRequestTime = lastSyncRequest + 1
        import model._
        // if (debug_transmit) committed.hashCode() else
        val rq = ChangeRequest(committedVersion, submit.zip(submitIds), state.mode, 0)
        request(0, api.request[ChangeRequest, ChangeResponse](s"/document/$docId/changes", rq), (value: ChangeResponse) => {
          lockObject.synchronized {
            flushInner()
            updateFromServer(value)
          }
        })
        true
      } else {
        false
      }
    } else {
      false
    }
  }

  def updating: Boolean = requesting

  def hasUncommited: Boolean = uncommitted.nonEmpty

  /**
    * sync with remote server
    */
  def sync(): Boolean = self.synchronized {
    if (model.debug_view) println("requested sync")
    lastSyncRequest = System.currentTimeMillis()
    if (!requesting) {
      return tryTopRequest()
    } else {
      false
    }
  }


  /**
    * update committed, committed version, uncommited, state
    */
  private def updateFromServer(success: ChangeResponse): Unit = {
    if (updateDisableUpdateBecauseLocalNodeDelete()) {
      if (model.debug_model) {
        println("ignore returned server stuff because we just performed a delete...")
      }
      // ignore it
    } else if (tempDisableRemoteStateUpdate) {
      disabledStateUpdates.append(success)
    } else {
      try {
        val take = success.acceptedLosersCount
        val winners = success.winners
        // it is ok to flatten the server updates, as what they rebase for is not uploaded to server yet
        val flatten = operation.Node.merge(winners.flatten)
        val (loser, remaining) = uncommitted.splitAt(take)
        // LATER handle conflict, modal handling of winner deletes loser
        val Rebased(cs0, (wp, lp)) = ot.Node.rebaseT(flatten, loser)
        committed = operation.Node.applyT(lp, operation.Node.apply(flatten, committed))
        committedVersion = success.finalVersion
        val Rebased(cs1, (wp0, uc)) = ot.Node.rebaseT(wp, remaining)
        uncommitted = uc//transaction.Node.mergeSingleOpTransactions(uc)
        uncommittedIds = Seq.empty // each time we got a new return, it is safe to discard the previous ids
        connection_.update(ConnectionStatus(success.serverStatus))
        if (wp0.nonEmpty) {
          val (last, from) = operation.Node.apply(wp0, state, enableModal)
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



  override def onExitSubMode(): Unit = {
    sourceEditorCommandBuffer.update("")
    state.mode match {
      case Some(model.mode.Node.Content(cur, sub: model.mode.Content.RichSubMode)) =>
        localChange(DocTransaction(state.copyContentMode(sub.modeBefore)))
      case Some(model.mode.Node.Content(cur, model.mode.Content.CodeInside(mode, pos))) =>
        localChange(DocTransaction(state.copyContentMode(model.mode.Content.CodeNormal(false))))
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

  override def onRefreshMode(): Unit = {
    if (model.debug_selection) println(s"refresh mode")
    updateState(state, Seq.empty, Seq.empty, Undoer.Local)
  }

  /**
    *
    */
  override def onMouseFocusOn(c: Node, ran: Option[IntRange], leftIsAnchor: Boolean, viewUpdated: Boolean, maybeNormal: Boolean): Boolean = {
    import model._
    if (model.debug_selection) println(s"focus on $ran")
    state.mode match {
      case Some(mode.Node.Content(cur, rich: mode.Content.Rich)) =>
        if (cur == c && ran.isEmpty) {
          localChange(DocTransaction(state.mode0))
          return true
        }
      case _ =>
    }
    val mo = state.node(c).content match {
      case c@data.Content.Rich(r) =>
        ran match {
          case Some(a) =>
            if (a.isEmpty) {
              if (!enableModal || state.isInsertal) {
                mode.Content.RichInsert(a.start)
              } else {
                mode.Content.RichNormal(r.rangeBefore(a.start))
              }
            } else {
              if (maybeNormal && enableModal && !state.isInsertal && r.rangeAfter(a.start).until == a.until) {
                mode.Content.RichNormal(a)
              } else {
                if (enableModal) {
                  mode.Content.RichVisual(r.rangeAfter(a.start), r.rangeBefore(a.until)).swap(leftIsAnchor)
                } else {
                  mode.Content.RichSelection(a.start, a.until).swap(leftIsAnchor)
                }
              }
            }
          case None =>
            c.defaultMode(enableModal)
        }
      case c: data.Content.Code =>
        val normalMode = enableModal && !state.isInsertal
        c.defaultMode(normalMode)
    }
    localChange(DocTransaction(Seq.empty, Some(model.mode.Node.Content(c, mo))))
    true
  }



  override def onVisualMode(mouseFirstContent: Node, node: Node): Unit = {
    localChange(DocTransaction(model.mode.Node.Visual(mouseFirstContent, node)))
  }

  override def onDeleteCurrentSelectionAndStartInsert(): Boolean = {
    state.mode match {
      case Some(model.mode.Node.Content(pos, rich: model.mode.Content.Rich)) =>
        rich match {
          case v: model.mode.Content.RichRange =>
            val trans = command.defaults.deleteRichNormalRange(state, _commandHandler, pos, v.merged, insert = true, noHistory = true)
            val ret = trans.transaction.nonEmpty
            localChange(trans)
            ret
          case a =>
            localChange(DocTransaction(state.copyContentMode(model.mode.Content.RichInsert(a.focus.start))))
            false
        }
      case _ => throw new IllegalArgumentException("Invalid command")
    }
  }

  override def onInsertRichTextAndViewUpdated(start: Int, end: Int, unicode: Unicode, backToNormal: Boolean, domInsertion: Int, mergeWithPrevious: Boolean): model.mode.Content.Rich = {
    import model._
    state.mode match {
      case Some(mode.Node.Content(cur, rich: mode.Content.Rich)) =>
        val before = state.node(cur).rich
        val afterSize = before.size + (end - start) + unicode.size
        val m = if (rich.isInstanceOf[mode.Content.RichInsert] && !backToNormal) {
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
            viewUpdated = true, editorUpdated = true), mergeWithPreviousLocal = mergeWithPrevious)
        m
      case _ => throw new IllegalStateException("Not supported")
    }

  }


  private val sourceEditorCommandBuffer = ObservableProperty("")
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

  override def onAttributeModified(attrs: Seq[SpecialChar], seq: Seq[Option[Unicode]]): Unit = {
    state.mode match {
      case Some(model.mode.Node.Content(node, model.mode.Content.RichAttributeSubMode(range, modeBefore))) =>
        val rich = state.node(node).rich
        localChange(DocTransaction(Seq(
          operation.Node.rich(node, operation.Rich.changeAttributeAt(rich, range.start - 1, attrs, seq))
        ), None))
      case _ => throw new IllegalStateException("What??")
    }
  }

  private var previousCopyId: String = Random.nextInt().toString

  // html, plain, and ct
  def onExternalCopyCut(isCut: Boolean): (Option[String], Option[String], Option[String]) = {
    val (trans, data) = defaults.yankSelection(state, _commandHandler, enableModal, isCut, reg = '*')
    localChange(trans)
    data.map {
      case Registerable.Unicode(a) =>
        (None, Some(a.str), None)
      case Registerable.Node(a, _) =>
        previousCopyId = Random.nextInt().toString
        val html = model.data.Node.toHtml(a)
        (Some(html), Some(html): Option[String], Some(previousCopyId))
      case Registerable.Text(a) =>
        previousCopyId = Random.nextInt().toString
        (Some(Text.toHtml(a)), Some(Text.toPlain(a)), Some(previousCopyId))
    }.getOrElse((None, None, None))
  }


  /**
    * currently code editors system copy/paste is not handled by this
    */
  override def onExternalPasteInRichEditor(html: Option[String], plain: Option[String], ct: Option[String]): Unit = {
    var done = false
    ct match {
      case Some(a) if a == previousCopyId =>
        done = true
      case _ =>
    }
    if (!done) {
      html match {
        case Some(h) if h.nonEmpty && platform.parseFromHtml != null =>
          done = true
          yank(platform.parseFromHtml(h), false, '*')
        case _ =>
      }
    }
    if (!done) {
      plain match {
        case Some(h) =>
          done = true
          try {
            if (Url.parse(h).isInstanceOf[AbsoluteUrl]) {
              val title = "?"
              yank(Registerable.Text(Seq(Text.Link(Seq(Text.Plain(Unicode(title))), Unicode(h)))), false, '*')
            } else {
              yank(Registerable.Unicode(Unicode(h)), false, '*')
            }
          } catch {
            case _: Throwable =>
              yank(Registerable.Unicode(Unicode(h)), false, '*')
          }
        case _ =>
      }
    }
    if (done) {
      if (state.isContent) {
        val pset = curRegister
        curRegister = '*'
        val command = if (getRegisterable().exists(_.isInstanceOf[Registerable.Node])) _commandHandler.yankPaste.putAfter else _commandHandler.yankPaste.putBefore
        localChange(command.action(state, 1, _commandHandler, None, None, None))
        curRegister = pset
      }
    }
  }


  private def applyFolds(folded0: DocState,
    unfold0: Set[cursor.Node],
    toggle0: Set[cursor.Node]
  ): (Map[UUID, Boolean], Map[cursor.Node, Boolean]) = {
    if (unfold0.isEmpty && toggle0.isEmpty) {
      (folded0.userFoldedNodes, Map.empty)
    } else {
      val toggle = toggle0.map(c => (c, folded0.node(c)))
      val unfold = unfold0.map(c => (c, folded0.node(c)))
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

  private def assertDisabledLocalUpdateConsistency(): Unit = {
    assert(uncommitted.lastOption.contains(Seq(disableUpdateBecauseLocalNodeDelete._1)))
  }

  override def undo(currentDoc: DocState): DocTransaction = {
    updateDisableUpdateBecauseLocalNodeDelete()
    if (disableUpdateBecauseLocalNodeDelete != null) {
      val insertOp = disableUpdateBecauseLocalNodeDelete._1.reverse(disableUpdateBecauseLocalNodeDelete._4.node)
      DocTransaction(Seq(insertOp), disableUpdateBecauseLocalNodeDelete._4.mode, tryMergeInsertOfDeleteRange = Some(disableUpdateBecauseLocalNodeDelete._1.r))
    } else {
      super.undo(currentDoc)
    }
  }

  def localChange(
    update0: DocTransaction,
    isSmartInsert: Boolean = false,
    mergeWithPreviousLocal: Boolean = false
  ): Unit = this.synchronized {
    var update: DocTransaction = update0
    if (debug_view) {
      println(s"local change $update")
    }

    val extra = update.extra match {
      case e@Some(_) => e
      case _ if !isSmartInsert => inputRuler.extraInputRuleOperation(state_, update.transaction)
      case _ => None
    }

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
            updateConnectionStatusBasedOnDisableStatus()
          }
      }
    }
    if (!justDisabled && disableUpdateBecauseLocalNodeDelete != null) {
      uncommitted.lastOption match {
        case Some(Seq(d@operation.Node.Delete(r))) =>
          if (update.tryMergeInsertOfDeleteRange.contains(r)) {
            update.transaction match {
              case operation.Node.Insert(at, childs) +: resTrans if childs == disableUpdateBecauseLocalNodeDelete._3 =>
                cutLastUndoHistory(Seq(d))
                val os = state_
                val zz = r.transformBeforeDeleted(os.zoom)
                val td = disableUpdateBecauseLocalNodeDelete._4
                state_ = td.copy(userFoldedNodes = state_.userFoldedNodes, zoom = zz, mode0 = model.mode.Node.Content(zz, td.node(zz).content.defaultMode(enableModal)))
                state_.consistencyCheck(enableModal)
                uncommitted = uncommitted.dropRight(1)
                val inverse = d.reverse(state.node)
                viewAdd = Seq((os, inverse))
                val to = d.r.transformBeforeDeleted(at)
                val trans = operation.Node.Move(d.r, to)
                update = update.copy(transaction = trans +: resTrans)
            }
          }
        case _ =>
      }
      if (update.transaction.nonEmpty && disableUpdateBecauseLocalNodeDelete != null) {
        disableUpdateBecauseLocalNodeDelete = null
        markAllAsNeedsClone()
        updateConnectionStatusBasedOnDisableStatus()
      }
    }
    val (res, ch) = applyFolds(state, update.unfoldBefore, update.toggleBefore)
    val (last0, from) = operation.Node.apply(update.transaction, state.copy(userFoldedNodes = res), enableModal)
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
      zoom = update.zoomAfter.getOrElse(last0.zoom))
    for (m <- update.viewMessagesBefore) {
      viewMessages_.onNext(m)
    }
    val needsSync = !update.nonTransactional
    if (needsSync) {
      updateState(last,
        from,
        viewAdd,
        update.undoType.getOrElse(Undoer.Local),
        isSmartInsert = isSmartInsert,
        viewUpdated = update.viewUpdated,
        editorUpdated = update.editorUpdated,
        fromUser = true,
        userFolds = ch, trace = update0.trace,
        mergeWithPreviousLocal = mergeWithPreviousLocal
      )
      if (update.transaction.nonEmpty) uncommitted = uncommitted :+ update.transaction
    }
    for (m <- update.viewMessagesAfter) {
      viewMessages_.onNext(m)
    }
    extra match {
      case Some(a) => localChange(a, isSmartInsert = true)
      case None => if (needsSync) sync()
    }
  }

  override def onKeyDown(k: Key): Boolean = _commandHandler.keyDown(k)

  override def onDoubleClick(): Unit = _commandHandler.onDoubleClick()

  override def flushBeforeMouseDown(): Unit = _commandHandler.flushBeforeMouseDown()

  override def searchState: Observable[SearchState] = searchHandler.searchState
}
