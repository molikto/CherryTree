package shared.ot

trait Doc[DATA, OPERATION <: OtOperation[DATA], CONFLICT, SELECTION] extends Ot[DATA, OPERATION, CONFLICT] {


  /** **
    *
    */
  override lazy val seqOt: Doc[Seq[DATA], SeqOperation[DATA, OPERATION], SeqConflict[DATA, CONFLICT], SeqSelection[SELECTION]] =
    new SeqDoc(this)

  def apply(op: OPERATION, sel: SELECTION): Option[SELECTION]
}
