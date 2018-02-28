package shared.ot





trait OtOperation {
  def isDeletion: Boolean
}


case class Rebased[T, CONFLICT](t: T, conflicts: Set[CONFLICT]) {
  def map[G](map: T => G) = Rebased(map(t), conflicts)
}

trait Ot[DATA, OPERATION <: OtOperation, CONFLICT] {


  def empty: DATA

  def apply(c: OPERATION, data: DATA): DATA

  def rebase(winner: OPERATION, loser: OPERATION): Rebased[OPERATION, CONFLICT]

  val dataSerializer: Serializer[DATA]
  val operationSerializer: Serializer[OPERATION]
}


