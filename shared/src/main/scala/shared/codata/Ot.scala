package shared.codata





trait Ot {

  type Data

  type Change

  type Conflict

  case class Rebased[T](t: T, conflicts: Set[Conflict]) {
    def map[G](map: T => G) = Rebased(map(t), conflicts)
  }

  def apply(c: Change, data: Data): Data

  def rebase(winner: Change, loser: Change): Rebased[Change]

}


