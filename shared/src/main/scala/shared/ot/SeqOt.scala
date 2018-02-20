package shared.ot



class SeqOt(val b: Ot) extends Ot {


  override type Data = Seq[b.Data]

  override def dataSerializer: Serializer[Data] = b.dataSerializer.seq

  /**
    * change
    */
  override type Operation = O

  override def optionSerializer: Serializer[O] = ???

  trait O extends OtOperation
  case class Insert(a: Int, content: Seq[b.Data]) extends O {
    override def isDeletion: Boolean = false
  }
  case class Delete(from: Int, to: Int) extends O {
    assert(to >= 0 && to >= from)
    override def isDeletion: Boolean = true
  }
  case class Child(i: Int, c: b.Operation) extends O {
    override def isDeletion: Boolean = c.isDeletion
  }

  /**
    * Change2
    */

  /**
    * conflicts
    */
  object C extends Enumeration {
    val Asymmetry, LoserDeletesWinner, WinnerDeletesLoser = Value
  }
  override type Conflict = C.Value
  import C._

  override def apply(c: O, data: Data): Data = ???

  override def rebase(winner: O, loser: O): Rebased[O] = ???

  override def empty: Seq[b.Data] = Seq.empty
}
