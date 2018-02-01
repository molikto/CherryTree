package shared.data

abstract sealed class RebaseConflict {
  def mirror = this
}

object RebaseConflict {

  def all = Set(Asymmetry, LoserDeletesWinner, WinnerDeletesLoser)

  def mirror(js: Set[RebaseConflict]): Set[RebaseConflict] = js.map(_.mirror)

  object Asymmetry extends RebaseConflict
  object LoserDeletesWinner extends RebaseConflict {
    override def mirror: RebaseConflict = WinnerDeletesLoser
  }
  object WinnerDeletesLoser extends RebaseConflict {
    override def mirror: RebaseConflict = LoserDeletesWinner
  }
}

object Rebased {
  def Free[T](t: T): Rebased[T] = Rebased(t, Set.empty)
  def Asymmetry[T](t: T): Rebased[T] = Rebased(t, Set(RebaseConflict.Asymmetry))
  def LoserDeletesWinner[T](t: T): Rebased[T] = Rebased(t, Set(RebaseConflict.LoserDeletesWinner))
  def WinnerDeletesLoser[T](t: T): Rebased[T] = Rebased(t, Set(RebaseConflict.WinnerDeletesLoser))
}



case class Rebased[T](result: T, ty: Set[RebaseConflict]) {
  def map[U](a: T => U): Rebased[U] = Rebased(a(result), ty)
}
