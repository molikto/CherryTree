package undoer

import doc.DocTransaction
import model._
import model.range.IntRange

import scala.collection.immutable.Queue
import scala.collection.mutable

object Undoer {

  sealed trait Type {

  }

  case object Local extends Type

  case object Remote extends Type

  case class Undo(a: IntRange) extends Type // points to the local or redo
  case class Redo(a: Int) extends Type // points to the undo
}

import Undoer._

trait UndoerInterface {

  def undo(): DocTransaction

  def redo(): DocTransaction
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

  private def size: Int = discarded + history_.size

  private def base: Int = discarded

  // local change consists of local, undo, redo
  def trackUndoerChange(trans: transaction.Node, ty: Type, modeBefore: Option[model.mode.Node], docBefore: data.Node): Unit = {
    // compress the history, by marking do/undo parts
    history_.lastOption match {
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
        a.docBefore = null
      case _ =>

    }
    val reverse = transaction.Node.reverse(docBefore, trans)
    val newItem = new HistoryItem(trans, reverse, docBefore, ty, modeBefore)
    history_ = history_ :+ newItem
  }

  private def undo(i: Int, isRedo: Boolean): DocTransaction = {
    val item = history(i)
    // TODO conflicts
    val (tt, pp) = ot.Node.rebaseT(item.reverse, after(i).map(_.trans)).t
    DocTransaction(tt, transaction.Node.transformSeq(pp, item.modeBefore), undoType = Some(Undo(IntRange(i))))
  }

  private def redo(i: Int): DocTransaction = {
    val item = history(i)
    // TODO conflicts
    val (tt, pp) = ot.Node.rebaseT(item.reverse, after(i).map(_.trans)).t
    DocTransaction(tt, transaction.Node.transformSeq(pp, item.modeBefore), undoType = Some(Redo(i)))
  }

  override def undo(): DocTransaction = {
    var i = size - 1
    while (i >= base) {
      val v = history(i)
      v.ty match {
        case Remote =>
          i -= 1
        case Local =>
          return undo(i, isRedo = false)
        case Undo(a) =>
          i = a.start - 1
        case Redo(a) =>
          return undo(i, isRedo = true)
      }
    }
    DocTransaction.empty
  }


  override def redo(): DocTransaction = {
    var i = size - 1
    while (i >= base) {
      val v = history(i)
      v.ty match {
        case Remote =>
          i -= 1
        case Local =>
          return DocTransaction.empty
        case Undo(_) =>
          redo(i)
        case Redo(a) =>
          i = a - 1
      }
    }
    DocTransaction.empty
  }
}
