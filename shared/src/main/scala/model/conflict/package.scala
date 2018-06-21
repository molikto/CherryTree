package model

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
    object Code {
      case class Content(u: Unicode) extends conflict.Content
      case class Lang(l: Option[String]) extends conflict.Content
    }
    object Paragraph {
      case class Content(u: conflict.Paragraph) extends conflict.Content
    }
  }



  class Node {

  }
  object Node {

  }


  type Paragraph = Unicode
}
