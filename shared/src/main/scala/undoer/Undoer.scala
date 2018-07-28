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

trait Undoer extends UndoerInterface {

  private var discarded = 0
  private var history_ : List[(transaction.Node, Type, Option[mode.Node])] = List.empty

  private def history(i: Int) = history_(i - discarded)
  private def after(i: Int) = history_.drop(i - discarded)

  private def size: Int = discarded + history_.size

  private def base: Int = discarded

  // local change consists of local, undo, redo
  def trackUndoerChange(trans: transaction.Node, ty: Type, modeBefore: Option[model.mode.Node]): Unit = {
    // compress the history, by marking do/undo parts
    history_ = history_ :+ (trans, ty, modeBefore)
  }

  private def undo(i: Int, isRedo: Boolean): DocTransaction = {
    val item = history(i)
    val t = transaction.Node.reverse(item._1)
    // TODO conflicts
    val (tt, pp) = ot.Node.rebaseT(t, after(i).map(_._1)).t
    DocTransaction(tt, transaction.Node.transform(pp, item._3), undoType = Some(Undo(IntRange(i))))
  }

  private def redo(i: Int): DocTransaction = {
    val item = history(i)
    val t = transaction.Node.reverse(item._1)
    // TODO conflicts
    val (tt, pp) = ot.Node.rebaseT(t, after(i).map(_._1)).t
    DocTransaction(tt, transaction.Node.transform(pp, item._3), undoType = Some(Redo(i)))
  }

  override def undo(): DocTransaction = {
    var i = size - 1
    while (i >= base) {
      val v = history(i)
      v._2 match {
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
      v._2 match {
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
