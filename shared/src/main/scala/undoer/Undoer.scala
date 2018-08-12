package undoer

import doc.{DocState, DocTransaction}
import model._
import model.data.Node
import model.range.IntRange

import scala.collection.immutable.Queue
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Undoer {

  sealed trait Type {

  }

  case object Local extends Type

  case object Remote extends Type

  case class Undo(a: Int, historyAfter: Seq[transaction.Node]) extends Type // points to the local or redo
  case class Redo(a: Int, historyAfter: Seq[transaction.Node]) extends Type // points to the undo
}

import Undoer._

trait UndoerInterface {

  def undo(currentDoc: DocState): DocTransaction

  def redo(currentDoc: DocState): DocTransaction
}

private[undoer] class HistoryItem(
  var trans: transaction.Node,
  var reverse: transaction.Node,
  var docBefore: DocState,
  val ty: Type,
  var undoer: (Seq[transaction.Node], Int) = null
) {

}

/**
  * our undoer is very very hacky!!!
  */
trait Undoer extends UndoerInterface {

  private var discarded = 0
  private var history_ : List[HistoryItem] = List.empty

  private def history(i: Int) = history_(i - discarded)
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

  def replaceUndoRedoPair(a: Int, items: Seq[transaction.Node]): Unit = {
    history_ = history_.take(a - discarded)
    // it is ok to discard all history with remote
    if (items.nonEmpty) {
      history_ = history_ :+ new HistoryItem(items.flatten, null, null, Remote, null)
    }
  }


  def debug_undoHistory = history_.zipWithIndex.map(p => (p._2 + discarded).toString + p._1.trans.toString() +" " + p._1.ty)

  private def size: Int = discarded + history_.size

  private def base: Int = discarded

  // all mode is changed to insert mode, then converted back to normal upon redo
  def convertMode(docBefore: Node, modeBefore: mode.Node): mode.Node= {
    modeBefore match {
      case v: model.mode.Node.Visual =>
        model.mode.Node.Content(v.fix, docBefore(v.fix).content match {
          case _: data.Content.Code => model.mode.Content.CodeNormal
          case _: data.Content.Rich => model.mode.Content.RichInsert(0)
        })
      case model.mode.Node.Content(n, a) => model.mode.Node.Content(n, a match {
          // LATER this is also hacky!!!
        case model.mode.Content.RichNormal(pos) =>
          val node = docBefore(n).rich
          model.mode.Content.RichInsert(pos.start)
        case model.mode.Content.RichVisual(fix, move) =>
          val node = docBefore(n).rich
          model.mode.Content.RichInsert(fix.start)
        case model.mode.Content.CodeInside =>
          model.mode.Content.CodeNormal
        case a => a
      })
    }
  }

  def convertBackMode(nodeNow: Node, modeNow: mode.Node): mode.Node= {
    modeNow match {
      case model.mode.Node.Content(n, a) => model.mode.Node.Content(n, a match {
        // LATER this is also hacky!!!
        case model.mode.Content.RichInsert(pos) =>
          val node = nodeNow(n).rich
          model.mode.Content.RichNormal(if (node.isEmpty) IntRange(0, 0) else node.rangeAfter(pos))
        case a => a
      })
      case _ => throw new IllegalArgumentException("That is impossible")
    }
  }

  // local change consists of local, undo, redo
  def trackUndoerChange(docBefore: DocState, trans: transaction.Node, ty: Type): Unit = {
    // compress the history, by marking do/undo parts
    if (trans.isEmpty && ty == Local) return
    def putIn(): Unit = {
      val reverse = transaction.Node.reverse(docBefore.node, trans)
      val newItem = new HistoryItem(trans, reverse, docBefore.copy(mode0 = convertMode(docBefore.node, docBefore.mode0)), ty)
      history_ = history_ :+ newItem
    }
    ty match {
      case u@Undo(a, pp) =>
        history(a).undoer = (pp, size)
        putIn()
      case Redo(a, items) =>
        replaceUndoRedoPair(a, items)
      case _ =>
        lastOption match {
          case Some(a) if a.ty == Local =>
            if (a.ty == Local) {
              transaction.Node.mergeForUndoer(trans, a.trans) match {
                case Some((merged, wait)) =>
                  a.trans = merged
                  a.reverse = transaction.Node.reverse(a.docBefore.node, a.trans)
                  return
                case _ =>
              }
            }
          case _ =>
        }
        putIn()
    }
  }



  private def undo(currentDoc: DocState, i: Int, isRedo: Boolean): DocTransaction = {
    val item = history(i)
    // TODO conflicts
    val (tt, pp) = ot.Node.rebaseT(item.reverse, after(i)).t
    val (applied, afrom) = operation.Node.apply(tt, currentDoc)
    if (isRedo) {
      history(item.ty.asInstanceOf[Undo].a).undoer = null
    }
    val (oldDocAsNowForModes, _) = operation.Node.apply(pp.flatten, item.docBefore)
    var zz = cursor.Node.parent(oldDocAsNowForModes.mode0.coverage)
    var break = false
    while (!break && zz.length > oldDocAsNowForModes.zoom.length) {
      if (!applied.folded(zz)) {
        zz = cursor.Node.parent(zz)
      } else {
        break = true
      }
    }
    DocTransaction(tt,
      Some(convertBackMode(applied.node, oldDocAsNowForModes.mode0)),
      zoomAfter = Some(zz),
      undoType = Some(if (isRedo) Redo(i, pp) else Undo(i, pp)))
  }

  protected def cutOneLocalHistory(assertTrans: transaction.Node): Unit = {
    assert(history_.last.ty == Local)
    assert(history_.last.trans == assertTrans)
    history_ = history_.dropRight(1)
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
