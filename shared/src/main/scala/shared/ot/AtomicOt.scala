package shared.ot



object AtomicOt {
  final case class Operation[T](a: T) extends OtOperation {
    override def isDestructive: Boolean = true
  }
  final case class Conflict[T](winner: T, loser: T)
}

/**
  * winner win atomic
  */
abstract class AtomicOt[DATA] extends Ot[DATA, AtomicOt.Operation[DATA], AtomicOt.Conflict[DATA]] {

  override def apply(c: AtomicOt.Operation[DATA], data: DATA): DATA = c.a

  /**
    * winner wins
    */
  override def rebase(
    winner: AtomicOt.Operation[DATA],
    loser: AtomicOt.Operation[DATA]
  ): Rebased[AtomicOt.Conflict[DATA], (Option[AtomicOt.Operation[DATA]], Option[AtomicOt.Operation[DATA]])] = {
    Rebased(Set(AtomicOt.Conflict(winner.a, loser.a)), (Some(winner), None))
  }
}


