package shared.data


object Rebased {
  def Free[T](t: T): Rebased[T] = Rebased(t, Set.empty)
  def Asymmetry[T](t: T): Rebased[T] = Rebased(t, Set(RebaseConflict.Asymmetry))
  def LoserDeletesWinner[T](t: T): Rebased[T] = Rebased(t, Set(RebaseConflict.LoserDeletesWinner))
  def WinnerDeletesLoser[T](t: T): Rebased[T] = Rebased(t, Set(RebaseConflict.WinnerDeletesLoser))
}



case class Rebased[T](result: T, ty: Set[RebaseConflict]) {
  def map[U](a: T => U): Rebased[U] = Rebased(a(result), ty)
}
