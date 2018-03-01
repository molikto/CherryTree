package shared.ot



sealed trait SeqOperation[T] extends OtOperation[Seq[T]] {
}

object SeqOperation {
  case class Add[T](at: Int, childs: Seq[T]) extends SeqOperation[T] {
    override def isDestructive: Boolean = false
  }
  case class Delete[T](from: Int, to: Int) extends SeqOperation[T] {
    assert(from >= 0 && to >= from)
    override def isDestructive: Boolean = true
  }
  case class Child[T, OP <: OtOperation[T]](at: Int, op: OP) extends SeqOperation[T] {
    override def isDestructive: Boolean = op.isDestructive
  }
}

sealed trait SeqConflict[T] {}

object SeqConflict {
  case class Asymmetry[T]() extends SeqConflict[T]
  case class WinnerDeletesLoser[T]() extends SeqConflict[T]
  case class LoserDeletesWinner[T]() extends SeqConflict[T]
}

class SeqOt[T] extends Ot[Seq[T], SeqOperation[T], SeqConflict[T]]{
  override def apply(c: SeqOperation[T], data: Seq[T]): Seq[T] = c match {
    case SeqOperation.Add(at, cs) => data.take(at) ++ cs ++ data.drop(at)
    case SeqOperation.Delete(from, to) => data.take(from) ++ data.drop(to + 1)
  }

  override def rebase(winner: SeqOperation[T], loser: SeqOperation[T]):
    Rebased[SeqConflict[T], (Option[SeqOperation[T]], Option[SeqOperation[T]])] = {
    ???
  }

  override val dataSerializer: Serializer[Seq[T]] = ???
  override val operationSerializer: Serializer[SeqOperation[T]] = ???
}



