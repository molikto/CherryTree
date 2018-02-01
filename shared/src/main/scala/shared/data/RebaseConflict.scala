
package shared.data

abstract sealed class RebaseConflict {
  def mirror: RebaseConflict = this
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
