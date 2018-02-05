package shared.codata

class ProductOt(
  val a: Ot,
  val b: Ot
) extends Ot {

  override type Data = (a.Data, b.Data)
  override type Change = Either[a.Change, b.Change]
  override type Conflict = Either[a.Conflict, b.Conflict]

  override def apply(c: Change, data: Data): Data =
    c match {
      case Left(da) => (a.apply(da, data._1), data._2)
      case Right(db) => (data._1, b.apply(db, data._2))
    }

  override def rebase(winner: Change, loser: Change): Rebased[Change] = {
    (winner, loser) match {
      case (Left(ca), Left(cl)) =>
        val t = a.rebase(ca, cl)
        Rebased(Left(t.t), t.conflicts.map(a => Left(a)))
      case (Right(ca), Right(cl)) =>
        val t = b.rebase(ca, cl)
        Rebased(Right(t.t), t.conflicts.map(a => Right(a)))
      case (_, _) => Rebased(loser, Set.empty) // free
    }
  }
}
