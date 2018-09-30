package model.conflict

abstract sealed class Node
object Node {
  case class Content(c: model.conflict.Content) extends Node
  case class ReplacedByWinner() extends Node
  case class ReplacedByLoser() extends Node
  case class Asymmetry() extends Node
  case class WinnerDeletesLoser() extends Node
  case class LoserDeletesWinner() extends Node
  case class WinnerMovesLoser() extends Node
}

