package shared.codata



class SeqOt(val b: Ot) extends Ot {
  override type Data = Seq[b.Data]

  /**
    * change
    */
  override type Change = SeqChange
  trait SeqChange
  case class Insert(a: Int, content: Seq[b.Data]) extends SeqChange
  case class Delete(from: Int, to: Int) extends SeqChange {
    assert(to >= 0 && to >= from)
  }
  case class Child(i: Int, c: b.Change) extends SeqChange

  /**
    * Change2
    */

  /**
    * conflicts
    */
  object SeqConflict extends Enumeration {
    val Asymmetry, LoserDeletesWinner, WinnerDeletesLoser = Value
  }
  override type Conflict = SeqConflict.Value
  import SeqConflict._


  override def apply(c: SeqChange, data: Seq[b.Data]): Seq[b.Data] = {

  }

  override def rebase(winner: SeqChange, loser: SeqChange): Rebased[SeqChange] = ???
}
