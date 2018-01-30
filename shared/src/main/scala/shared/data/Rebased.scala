package shared.data

abstract sealed class RebaseType {
  def mirror = this
}
object RebaseType {
  def mirror(js: Set[RebaseType]): Set[RebaseType] = js.map(_.mirror)

  object Asymmetry extends RebaseType
  object LoserDeletesWinner extends RebaseType {
    override def mirror: RebaseType = WinnerDeletesLoser
  }
  object WinnerDeletesLoser extends RebaseType {
    override def mirror: RebaseType = LoserDeletesWinner
  }
}

object Rebased {
  def Free[T](t: T): Rebased[T] = Rebased(t, Set.empty)
  def Asymmetry[T](t: T): Rebased[T] = Rebased(t, Set(RebaseType.Asymmetry))
  def LoserDeletesWinner[T](t: T): Rebased[T] = Rebased(t, Set(RebaseType.LoserDeletesWinner))
  def WinnerDeletesLoser[T](t: T): Rebased[T] = Rebased(t, Set(RebaseType.WinnerDeletesLoser))
}



case class Rebased[T](result: T, ty: Set[RebaseType]) {
  def map[U](a: T => U): Rebased[U] = Rebased(a(result), ty)
}
