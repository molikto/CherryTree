package shared.ot





object Ot {

}

trait Ot {

  val ty: OtType

  type Data

  type Operation

  type Conflict

  case class Rebased[T](t: T, conflicts: Set[Conflict]) {
    def map[G](map: T => G) = Rebased(map(t), conflicts)
  }

  def apply(c: Operation, data: Data): Data

  def rebase(winner: Operation, loser: Operation): Rebased[Operation]

}


