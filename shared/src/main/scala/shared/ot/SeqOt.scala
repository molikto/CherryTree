package shared.ot



object SeqOt {

}

class SeqOt(val b: Ot) extends Ot {


  override type Data = Seq[b.Data]

  /**
    * change
    */
  override type Operation = O
  trait O
  case class Insert(a: Int, content: Seq[Data]) extends O
  case class Delete(from: Int, to: Int) extends O {
    assert(to >= 0 && to >= from)
  }
  case class Child(i: Int, c: b.Operation) extends O

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

}
