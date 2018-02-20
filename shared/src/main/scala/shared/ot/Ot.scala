package shared.ot





object Ot {

}

trait Ot {
  /**
    * main types
    */
  type Data

  type Operation <: OtOperation

  type Conflict

  case class Rebased[T](t: T, conflicts: Set[Conflict]) {
    def map[G](map: T => G) = Rebased(map(t), conflicts)
  }

  def empty: Data

  def apply(c: Operation, data: Data): Data

  def rebase(winner: Operation, loser: Operation): Rebased[Operation]


  def dataSerializer: Serializer[Data]
  def optionSerializer: Serializer[Operation]
}


