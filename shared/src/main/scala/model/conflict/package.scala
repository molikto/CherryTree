package model

/**
  * asymmetry handling, if someone in
  */
package object conflict {

  class Unicode

  object Unicode {
    case class Asymmetry() extends Unicode
    case class WinnerDeletesLoser() extends Unicode
    case class LoserDeletesWinner() extends Unicode
    case class WinnerMovesLoser() extends Unicode
  }

  abstract sealed class Content()
  object Content {
    case class CodeContent(u: Unicode) extends conflict.Content
    case class CodeLang(l: String) extends conflict.Content
    case class Rich(u: conflict.Paragraph) extends conflict.Content
  }



  abstract sealed class Node
  object Node {
    case class Content(c: conflict.Content) extends Node
    case class ReplacedByWinner() extends Node
    case class ReplacedByLoser() extends Node
    case class Asymmetry() extends Node
    case class WinnerDeletesLoser() extends Node
    case class LoserDeletesWinner() extends Node
    case class WinnerMovesLoser() extends Node
  }


  type Paragraph = Unicode
}
