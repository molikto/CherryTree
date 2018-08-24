package undoer

import doc.{DocState, DocTransaction}
import model.{ot, _}
import model.data.Node
import model.range.IntRange
import settings.Settings
import undoer.Undoer.Remote

import scala.collection.immutable.Queue
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Undoer {

  sealed trait Type {

  }

  case object Local extends Type

  case object Remote extends Type

// points to the local or redo
  case class Undo(a: Int, historyAfter: Seq[transaction.Node]) extends Type {
  override def toString: String = "Undo"
}
  // points to the undo
  case class Redo(a: Int, historyAfter: Seq[transaction.Node]) extends Type  {
    override def toString: String = "Redo"
  }
}

import Undoer._

trait UndoerInterface {

  def undo(currentDoc: DocState): DocTransaction

  def redo(currentDoc: DocState): DocTransaction
}

private[undoer] class HistoryItem(
  var trans: transaction.Node,
  var docBefore: DocState,
  val ty: Type,
  var undoer: (Seq[transaction.Node], Int) = null
) {

  def reverse: transaction.Node = ot.Node.reverse(docBefore.node, trans)
}

/**
  * our undoer is very very hacky!!!
  */
trait Undoer extends UndoerInterface with Settings {

  private var discarded = 0
  private val history_ : ArrayBuffer[HistoryItem] = new ArrayBuffer()

  private def history(i: Int) = history_(i - discarded)

  private def history(i: Int, b: HistoryItem): Unit = history_(i - discarded) = b

  private def after(i: Int): Seq[transaction.Node] = {
    val bf = new ArrayBuffer[transaction.Node]()
    var s = i - discarded + 1
    while (s < size) {
      val item = history(s)
      if (item.undoer != null) {
        bf.appendAll(item.undoer._1)
        s = item.undoer._2 + 1
      } else {
        bf.append(item.trans)
        s += 1
      }
    }
    bf
  }

  private def lastOption = history_.lastOption

  private def replaceUndoRedoPair(a: Int, items: Seq[transaction.Node]): Unit = {
    val from = a - discarded
    history_.remove(from, history_.size - from)
    // it is ok to discard all history with remote
    if (items.nonEmpty) {
      append(new HistoryItem(items.flatten, null, Remote, null))
    }
  }

  protected def cutOneLocalHistory(assertTrans: transaction.Node): Unit = {
    assert(history_.last.ty == Local, "history is not local....?")
    assert(history_.last.trans == assertTrans, s"trans different ${history_.last.trans} $assertTrans")
    removeOne()
  }

  private def append(newItem: HistoryItem) = {
    history_.append(newItem)
  }

  private def removeOne() = {
    history_.remove(history_.size - 1)
  }

  private def compatHistory(whitespace: Boolean): Unit = {
    while (size >= base + 2) {
      val last = history(size - 1)
      val before = history(size - 2)
      if (before == lastCompatItem) return
      if (before.ty == Local && last.ty == Local) {
        transaction.Node.mergeSingleOpTransactions(last.trans, before.trans, whitespace) match {
          case Some(trans) =>
            assert(before.undoer == null && last.undoer == null)
            before.trans = trans
            removeOne()
          case None => return
        }
      } else {
        return
      }
    }
  }

  private def sinkRemote() = {
    assert(lastOption.get.ty == Remote)
    var at = size - 1
    while (at > base) {
      val remote = history(at)
      val before = history(at - 1)
      if (before.ty == Local) {
        ot.Node.swap(before.docBefore.node, before.trans, remote.trans) match {
          case Some((r, l)) =>
            assert(before.undoer == null && remote.undoer == null)
            val docBefore = before.docBefore
            remote.docBefore = docBefore
            remote.trans = r
            before.trans = l
            before.docBefore = operation.Node.apply(r, docBefore, enableModal)._1
            history(at, before)
            history(at - 1, remote)
            at -= 1
          case None =>
            at = -1
        }
      } else {
        at = -1
      }
    }
    if (at == base) { // we can happily forget this change...
      discarded += 1
      history_.remove(0, 1)
    }
  }


  def debug_undoHistory = history_.zipWithIndex.map(p => (p._2 + discarded, p._1.trans, p._1.ty.toString))

  private def size: Int = discarded + history_.size

  private def base: Int = discarded

  // all mode is changed to insert mode, then converted back to normal upon redo
  def convertMode(docBefore: Node, modeBefore: mode.Node): mode.Node = {
    if (enableModal) {
      modeBefore match {
        case v: model.mode.Node.Visual =>
          model.mode.Node.Content(v.fix, docBefore(v.fix).content.defaultMode(enableModal))
        case model.mode.Node.Content(n, a) => model.mode.Node.Content(n, {
          def rec(c: model.mode.Content): model.mode.Content = c match {
            // LATER this is also hacky!!!
            case v@model.mode.Content.RichVisual(fix, move) =>
              v.collapse(enableModal)
            case model.mode.Content.RichAttributeSubMode(range, modeBefore) =>
              rec(modeBefore)
            case sub@model.mode.Content.RichCodeSubMode(range, code, modeBefore) =>
              sub
            case a => a
          }
          rec(a)
        })
      }
    } else {
      modeBefore
    }
  }

  def convertInsertMode(nodeNow: Node, modeNow: mode.Node): mode.Node= {
    modeNow match {
      case model.mode.Node.Content(n, a) => model.mode.Node.Content(n, a match {
        // LATER this is also hacky!!!
        case model.mode.Content.RichInsert(pos) =>
          val node = nodeNow(n).rich
          model.mode.Content.RichNormal(if (node.isEmpty) IntRange(0, 0) else node.rangeAfter(pos))
        case sub@model.mode.Content.RichCodeSubMode(node, a, content) =>
          sub.copy(code = sub.code.copy(mode = "normal"))
        case sub: model.mode.Content.CodeInside =>
          sub.copy(mode = "normal")
        case a => a
      })
      case _ => throw new IllegalArgumentException("That is impossible")
    }
  }

  private var breaking = false



  // local change consists of local, undo, redo
  def trackUndoerChange(docBefore: DocState, docAfter: DocState, trans: transaction.Node, ty: Type, isExtra: Boolean): Unit = {
    def putIn(): Unit = {
      val newItem = new HistoryItem(trans, docBefore.copy(mode0 = convertMode(docBefore.node, docBefore.mode0)), ty)
      append(newItem)
    }
    ty match {
      case u@Undo(a, pp) =>
        history(a).undoer = (pp, size)
        putIn()
      case Redo(a, items) =>
        replaceUndoRedoPair(a, items)
      case Remote =>
        if (trans.nonEmpty) {
          putIn()
          sinkRemote() // LATER do this while undo, to save some space
        }
      case Local =>
        if (trans.nonEmpty) {
          if (previousIsExtra) {
            previousIsExtra = false
            compatHistory(true)
          }
          putIn()
          if (!isExtra) {
            compatHistory(true)
          } else {
            previousIsExtra = true
          }
        }
    }
    if (breaking && !docAfter.breakWhiteSpaceInserts) {
      compatHistory(false)
      lastCompatItem = lastOption.orNull
    }
    breaking = docAfter.breakWhiteSpaceInserts
  }

  private var previousIsExtra = false
  private var lastCompatItem: HistoryItem = null

  private def undo(currentDoc: DocState, i: Int, isRedo: Boolean): DocTransaction = {
    val item = history(i)
    // TODO conflicts
    val (tt, pp) = ot.Node.rebaseT(item.reverse, after(i)).t
    val (applied, afrom) = operation.Node.apply(tt, currentDoc, enableModal)
    if (isRedo) {
      history(item.ty.asInstanceOf[Undo].a).undoer = null
    }
    val (oldDocAsNowForModes, _) = operation.Node.apply(pp.flatten, item.docBefore, enableModal)
    val coverage = oldDocAsNowForModes.mode0.coverage // can only be normal
    val zzz = if (cursor.Node.contains(applied.zoom, coverage) && !applied.viewAsHidden(coverage)) {
      None
    } else {
      var break = false
      var zz = cursor.Node.parent(coverage)
      while (!break && zz.length > oldDocAsNowForModes.zoom.length) {
        if (!applied.folded(zz)) {
          zz = cursor.Node.parent(zz)
        } else {
          break = true
        }
      }
      Some(zz)
    }
    val changeZoom = zzz.map(z => applied.copy(zoom = z)).getOrElse(applied)
    val mode = oldDocAsNowForModes.mode0 match {
      case model.mode.Node.Visual(a, b) =>
        model.mode.Node.Visual(changeZoom.notHiddenParent(a), changeZoom.notHiddenParent(b))
      case a => a
    }
    DocTransaction(tt,
      Some(if (currentDoc.isInsert || !enableModal) mode else convertInsertMode(applied.node, mode)),
      zoomAfter = zzz,
      undoType = Some(if (isRedo) Redo(i, pp) else Undo(i, pp)))
  }


  override def undo(currentDoc: DocState): DocTransaction = {
    var i = size - 1
    while (i >= base) {
      val v = history(i)
      v.ty match {
        case Remote =>
          i -= 1
        case Local =>
          return undo(currentDoc, i, false)
        case Undo(a, _) =>
          i = a - 1
        case r: Redo =>
          throw new IllegalArgumentException("You should not see redo here")

      }
    }
    DocTransaction.empty
  }


  override def redo(currentDoc: DocState): DocTransaction = {
    var i = size - 1
    while (i >= base) {
      val v = history(i)
      v.ty match {
        case Remote =>
          i -= 1
        case Local =>
          return DocTransaction.empty
        case Undo(_, _) =>
          return undo(currentDoc, i, true)
        case r: Redo =>
          throw new IllegalArgumentException("You should not see redo here")
      }
    }
    DocTransaction.empty
  }
}
