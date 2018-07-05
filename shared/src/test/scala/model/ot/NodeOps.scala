package model.ot

import model._
import model.range.IntRange

object NodeOps {


  def isAsymmetry(n: conflict.Content): Boolean = n match {
    case conflict.Content.Code.Content(conflict.Unicode.Asymmetry()) => true
    case conflict.Content.Paragraph.Content(conflict.Unicode.Asymmetry()) => true
    case _ => false
  }

  def isAsymmetry(a: conflict.Node): Boolean = a match {
    case conflict.Node.Asymmetry() => true
    case conflict.Node.Content(n) => isAsymmetry(n)
    case _ => false
  }

  def isAsymmetry(op: Set[conflict.Node]): Boolean = op.exists(a => isAsymmetry(a))


  def insertNode(at: Seq[Int], content: String): operation.Node = {
    operation.Node.Insert(at, Seq(data.Node(data.Content.Code(data.Unicode(content), None), Seq.empty)))
  }

  def insertContent(at: Seq[Int], p: Int, content: String): operation.Node = {
    operation.Node.Content(at, operation.Content.Code.Content(operation.Unicode.Insert(p, data.Unicode(content))))
  }

  def deleteContent(at: Seq[Int], from: Int, len: Int): operation.Node = {
    operation.Node.Content(at, operation.Content.Code.Content(operation.Unicode.Delete(IntRange(from, from + len - 1))))
  }

  def deleteNode(at: Seq[Int]): operation.Node = {
    operation.Node.Delete(range.Node(at))
  }
}

