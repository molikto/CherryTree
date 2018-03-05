package shared.ot




trait Doc[DATA, OPERATION <: OtOperation[DATA], CONFLICT, SELECTION] extends Ot[DATA, OPERATION, CONFLICT] {

  def apply(op: OPERATION, sel: SELECTION): Option[SELECTION]
}
