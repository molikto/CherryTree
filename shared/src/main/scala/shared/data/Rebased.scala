package shared.data


sealed class RebaseType {
}

object RebaseType {
  object Free extends RebaseType
  /**
    * in favor of winner, e.g. insert at same position, winner's text is treated as fire first
    */
  object Asymmetry extends RebaseType

  abstract class Conflict extends RebaseType

  /**
    * in case the loser's action deletes winner's unseen (by loser) action
    */
  object LoserDeletesWinner extends Conflict

  /**
    * in case the winner's action deletes new loser's insert
    */
  object WinnerDeletesLoser extends Conflict
}

object Rebased {
  def Free[T](t: T): Rebased[T] = Rebased(t, RebaseType.Free)
  def Asymmetry[T](t: T): Rebased[T] = Rebased(t, RebaseType.Asymmetry)
  def LoserDeletesWinner[T](t: T): Rebased[T] = Rebased(t, RebaseType.LoserDeletesWinner)
  def WinnerDeletesLoser[T](t: T): Rebased[T] = Rebased(t, RebaseType.WinnerDeletesLoser)
}



case class Rebased[T](result: T, ty: RebaseType) {

  def map[U](a: T => U): Rebased[U] = Rebased(a(result), ty)
}
