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
  case class Undo(range: IntRange) extends Type
  case class Redo(a: Int) extends Type
}

import Undoer._

trait UndoerInterface {

  def undo(): DocTransaction
  def redo(): DocTransaction
}

trait Undoer extends UndoerInterface {

  private var history: List[(transaction.Node, Type, Option[mode.Node])] = List.empty

  // local change consists of local, undo, redo
  def trackUndoerChange(trans: transaction.Node, ty: Type, mode: Option[model.mode.Node]): Unit = {
    // compress the history, by marking do/undo parts
    history = history :+ (trans, ty, mode)
  }

  override def undo(): DocTransaction = {
    DocTransaction.empty

//    for (a <- history.zipWithIndex.reverse) {
//      a._1._2 match {
//        case Local
//      }
//    }
  }

  override def redo(): DocTransaction = {
    DocTransaction.empty
  }
}
