package shared.ot

sealed trait OtType
object OtType {
  case class Atomic(name: String) extends OtType
  case class Composite(applier: Atomic, args: Seq[OtType]) extends OtType
}
