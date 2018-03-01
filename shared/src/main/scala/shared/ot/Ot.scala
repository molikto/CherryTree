package shared.ot


trait OtOperation {
  def isDestructive: Boolean
}


case class Rebased[CONFLICT, T](conflicts: Set[CONFLICT], t: T) {
  def map[G](map: T => G) = Rebased(conflicts, map(t))
}

trait Ot[DATA, OPERATION <: OtOperation, CONFLICT] {


  //def empty: DATA

  def apply(c: OPERATION, data: DATA): DATA

  def rebase(winner: OPERATION, loser: OPERATION): Rebased[CONFLICT, (Option[OPERATION], Option[OPERATION])]

  val dataSerializer: Serializer[DATA]
  val operationSerializer: Serializer[OPERATION]
}


