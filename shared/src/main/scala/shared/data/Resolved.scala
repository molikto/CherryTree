package shared.data


sealed class ResolveType {
  object Free extends ResolveType
  /**
    * in favor of winner, e.g. insert at same position, winner's text is treated as fire first
    */
  object Asymmetry extends ResolveType

  abstract class Conflict extends ResolveType

  /**
    * in case the loser's action deletes winner's unseen (by loser) action
    */
  object LoserDeletesWinner extends Conflict

  /**
    * in case the winner's action deletes new loser's insert
    */
  object WinnerDeletesLoser extends Conflict
}



case class Resolved[T](result: T, ty: ResolveType) {

}
