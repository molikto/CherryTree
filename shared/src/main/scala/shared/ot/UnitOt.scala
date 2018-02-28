package shared.ot


sealed trait UnitOperation
object UnitOperation {
  object Unit extends UnitOperation with OtOperation {
    override def isDeletion: Boolean = false
  }
}

object UnitOt extends Ot[Unit, UnitOperation, Unit] {

  override def empty: Unit = Unit

  override def apply(c: UnitOperation, data: Unit): Unit = Unit

  override def rebase(winner: UnitOperation, loser: UnitOperation): Rebased[UnitOperation] =
    Rebased(UnitOperation.Unit, Set.empty)

  override val dataSerializer: Serializer[Unit] = new Serializer[Unit] {
    override def parse(t: Array[Byte]): Unit = Unit
    override def serialize(t: Unit): Array[Byte] = Array.emptyByteArray
  }

  override val operationSerializer: Serializer[UnitOperation] = new Serializer[UnitOperation] {
    override def parse(t: Array[Byte]): UnitOperation = UnitOperation.Unit
    override def serialize(t: UnitOperation): Array[Byte] = Array.emptyByteArray
  }
}
