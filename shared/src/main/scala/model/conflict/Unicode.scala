package model.conflict


class Unicode

object Unicode {
  case class Asymmetry() extends Unicode
  case class WinnerDeletesLoser() extends Unicode
  case class LoserDeletesWinner() extends Unicode
  case class WinnerMovesLoser() extends Unicode
}
