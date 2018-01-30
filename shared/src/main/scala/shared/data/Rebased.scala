package shared.data


sealed class RebaseType {
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



case class Rebased[T](result: T, ty: RebaseType) {

}
