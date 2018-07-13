package model

import model.range.IntRange

package object mode {
  sealed trait Content
  object Content {
    case class Insertion(pos: Int) extends Content {
    }
    /**
      * second parameter is a range because selection is not just one codepoint
      *
      * press copy in visual mode you get different result in different editors
      * we adapt one...
      */
    case class Normal(range: IntRange) extends Content
    object Normal {
      val empty: Normal = Normal(IntRange(0, 0))
    }
    case class Visual(fix: IntRange, move: IntRange) extends Content
  }

  sealed trait Node
  object Node {
    val empty = Content(cursor.Node.Root, mode.Content.Normal.empty)

    case class Content(node: cursor.Node, a: mode.Content) extends Node
    case class Visual(fix: cursor.Node, move: cursor.Node) extends Node


    val pickler: Pickler[Node] = new Pickler[Node]() {

      override def pickle(obj: Node)(implicit state: PickleState): Unit = {
        import state.enc._
        obj match {
          case Content(node, mode.Content.Insertion(pos)) =>
            writeInt(0)
            writeIntArray(node.toArray)
            writeInt(pos)
          case Content(node, mode.Content.Normal(range)) =>
            writeInt(1)
            writeIntArray(node.toArray)
            IntRange.pickler.pickle(range)
          case Content(node, mode.Content.Visual(fix, move)) =>
            writeInt(2)
            writeIntArray(node.toArray)
            IntRange.pickler.pickle(fix)
            IntRange.pickler.pickle(move)
          case Visual(fix, move) =>
            writeInt(3)
            writeIntArray(fix.toArray)
            writeIntArray(move.toArray)
        }
      }

      override def unpickle(implicit state: UnpickleState): Node = {
        import state.dec._
        readInt match {
          case 0 => Content(readIntArray, mode.Content.Insertion(readInt))
          case 1 => Content(readIntArray, mode.Content.Normal(IntRange.pickler.unpickle))
          case 2 => Content(readIntArray, mode.Content.Visual(IntRange.pickler.unpickle, IntRange.pickler.unpickle))
          case 3 => Visual(readIntArray, readIntArray)
        }
      }
    }
  }
}
