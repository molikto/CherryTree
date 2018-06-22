package model.ot

import model._
import model.operation.Node
import model.range.IntRange

object NodeOps {


  def isAsymmetry(n: SeqConflict[Node, Node.Conflict]): Boolean = n match {
    case SeqConflict.Asymmetry() => true
    case SeqConflict.Child(k) => isAsymmetry(k)
    case _ => false
  }

  def isAsymmetry(a: conflict.Node): Boolean = a match {
    case Node.Conflict.Content(c) => c match {
      case OtStringConflict.Asymmetry() => true
      case _ => false
    }
    case Node.Conflict.Childs(n) => isAsymmetry(n)
  }

  def isAsymmetry(op: Set[conflict.Node]): Boolean = op.exists(a => isAsymmetry(a))


  def insertNode(at: Seq[Int], content: String): operation.Node = {
    operation.Node.Insert(at, Seq(data.Node(data.Content.Code(Unicode(content), None), Seq.empty)))
  }

  def insertContent(at: Seq[Int], p: Int, content: String): operation.Node = {
    operation.Node.Content(at, operation.Content.Code.Content(operation.Unicode.Insert(p, Unicode(content))))
  }

  def deleteContent(at: Seq[Int], from: Int, len: Int): operation.Node = {
    operation.Node.Content(at, operation.Content.Code.Content(operation.Unicode.Delete(IntRange(from, from + len - 1))))
  }

  def deleteNode(at: Seq[Int]): operation.Node = {
    operation.Node.Delete(range.Node(at))
  }
}

