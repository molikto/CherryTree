package shared.codata


class FixOt[T[_]](a: Ot) extends Ot {
  override type Data = T[a.Data]
  override type Change = T[a.Change]
  override type Conflict = T[a.Conflict]


  override def apply(c: Change, data: Data): Data = {
    a.app
  }

  override def rebase(winner: Change, loser: Change): Rebased[Change]
}
