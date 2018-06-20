package shared.operation

import shared.operation.Type.Type

trait Seq[T, OP <: Operation[T]] extends Operation[scala.Seq[T]] {
}


object Seq {
  case class Insert[T, OP <: Operation[T]](at: Int, childs: scala.Seq[T]) extends Seq[T, OP] {
    override def ty: Type = Type.Add
  }
  case class Delete[T, OP <: Operation[T]](start: Int, endInclusive: Int) extends Seq[T, OP] {
    assert(start >= 0 && endInclusive >= start)
    override def ty: Type = Type.Delete
    def size: Int = endInclusive - start + 1
  }
  case class Child[T, OP <: Operation[T]](at: Int, op: OP) extends Seq[T, OP] {
    override def ty: Type = op.ty
  }
}

