package shared.ot



sealed trait Content

object Content {
  case class Plain(obj: String) extends Content

  sealed trait Operation
  object Operation {

  }
}

trait Doc[DATA, OPERATION <: OtOperation[DATA], CONFLICT, SELECTION] extends Ot[DATA, OPERATION, CONFLICT] {

  def apply(op: OPERATION, sel: SELECTION): Option[SELECTION]
}
