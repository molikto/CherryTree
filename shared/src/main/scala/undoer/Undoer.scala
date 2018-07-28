package undoer

import doc.DocTransaction
import model._
import model.data.Node
import model.range.IntRange

import scala.collection.immutable.Queue
import scala.collection.mutable

object Undoer {

  sealed trait Type {

  }

  case object Local extends Type

  case object Remote extends Type

  case class Undo(a: Int) extends Type // points to the local or redo
  case class Redo(a: Int, historyAfter: Seq[transaction.Node]) extends Type // points to the undo
}

import Undoer._

trait UndoerInterface {

  def undo(currentDoc: Node): DocTransaction

  def redo(currentDoc: Node): DocTransaction
}

private[undoer] class HistoryItem(
  var trans: transaction.Node,
  var reverse: transaction.Node,
  var docBefore: data.Node,
  val ty: Type,
  val modeBefore: Option[mode.Node]
) {

}

trait Undoer extends UndoerInterface {

  private var discarded = 0
  private var history_ : List[HistoryItem] = List.empty

  private def history(i: Int) = history_(i - discarded)
  private def after(i: Int) = history_.drop(i - discarded + 1)
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
  def convertMode(docBefore: Node, modeBefore: Option[mode.Node]): Option[mode.Node] = {
    modeBefore.map {
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

  def convertBackMode(nodeNow: Node, modeNow: Option[mode.Node]): Option[mode.Node] = {
    modeNow.map {
      case model.mode.Node.Content(n, a) => model.mode.Node.Content(n, a match {
        // LATER this is also hacky!!!
        case model.mode.Content.RichInsert(pos) =>
          val node = nodeNow(n).rich
          model.mode.Content.RichNormal(node.rangeAfter(pos))
        case a => a
      })
      case _ => throw new IllegalArgumentException("That is impossible")
    }
  }

  // local change consists of local, undo, redo
  def trackUndoerChange(trans: transaction.Node, ty: Type, modeBefore: Option[model.mode.Node], docBefore: data.Node): Unit = {
    // compress the history, by marking do/undo parts
    if (trans.isEmpty && ty == Local) return
    ty match {
      case Redo(a, items) =>
        replaceUndoRedoPair(a, items)
      case _ =>
        lastOption match {
          case Some(a) if a.ty == Local =>
            if (a.ty == Local) {
              transaction.Node.merge(trans, a.trans) match {
                case Some(merged) =>
                  a.trans = merged
                  a.reverse = transaction.Node.reverse(a.docBefore, a.trans)
                  return
                case _ =>
              }
            }
          case _ =>
        }
        val reverse = transaction.Node.reverse(docBefore, trans)
        val newItem = new HistoryItem(trans, reverse, docBefore, ty, convertMode(docBefore, modeBefore))
        history_ = history_ :+ newItem
    }
  }



  private def undo(currentDoc: Node, i: Int, isRedo: Boolean): DocTransaction = {
    val item = history(i)
    // TODO conflicts
    val (tt, pp) = ot.Node.rebaseT(item.reverse, after(i).map(_.trans)).t
    val applied = operation.Node.apply(tt, currentDoc)
    DocTransaction(tt,
      convertBackMode(applied, transaction.Node.transformSeq(pp, item.modeBefore)),
      undoType = Some(if (isRedo) Redo(i, pp) else Undo(i)),
      handyAppliedResult = Some(applied))
  }

  override def undo(currentDoc: Node): DocTransaction = {
    var i = size - 1
    while (i >= base) {
      val v = history(i)
      v.ty match {
        case Remote =>
          i -= 1
        case Local =>
          return undo(currentDoc, i, false)
        case Undo(a) =>
          i = a - 1
        case r: Redo =>
          throw new IllegalArgumentException("You should not see redo here")

      }
    }
    DocTransaction.empty
  }


  override def redo(currentDoc: Node): DocTransaction = {
    var i = size - 1
    while (i >= base) {
      val v = history(i)
      v.ty match {
        case Remote =>
          i -= 1
        case Local =>
          return DocTransaction.empty
        case Undo(_) =>
          return undo(currentDoc, i, true)
        case r: Redo =>
          throw new IllegalArgumentException("You should not see redo here")
      }
    }
    DocTransaction.empty
  }
}
