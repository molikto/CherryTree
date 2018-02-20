package shared.ot


object UnitOt extends Ot {
  /**
    * main types
    */
  override type Data = Unit

  trait O extends OtOperation
  case object OO extends O {
    override def isDeletion: Boolean = false
  }
  override type Operation = O
  override type Conflict = Unit

  override def empty: Unit = Unit

  override def apply(c: O, data: Unit): Unit = Unit

  override def rebase(winner: O, loser: O): Rebased[O] = Rebased(OO, Set.empty)

  override val dataSerializer: Serializer[Unit] = new Serializer[Unit] {
    override def parse(t: Array[Byte]): Unit = Unit
    override def serialize(t: Unit): Array[Byte] = Array.emptyByteArray
  }

  override def optionSerializer: Serializer[O] = new Serializer[O] {
    override def parse(t: Array[Byte]): O = OO
    override def serialize(t: O): Array[Byte] = Array.emptyByteArray
  }
}
