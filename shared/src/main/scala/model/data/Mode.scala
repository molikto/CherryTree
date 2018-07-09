package model.data

import model.cursor
import model._
import model.range.IntRange


case class ClientState(node: Node, mode: Mode)

abstract sealed class Mode {
}

object Mode {
  case class ContentInsertion(node: cursor.Node, pos: Int) extends Mode
  /**
    * second parameter is a range because selection is not just one codepoint
    *
    * press copy in visual mode you get different result in different editors
    * we adapt one...
    */
  case class ContentNormal(node: cursor.Node, range: IntRange) extends Mode
  object ContentNormal {
    val empty = ContentNormal(cursor.Node.Root, IntRange(0, 0))
  }
  case class ContentVisual(node: cursor.Node, fix: Int, move: Int) extends Mode
  case class NodeVisual(fix: cursor.Node, move: cursor.Node) extends Mode

  val pickler: Pickler[Mode] = new Pickler[Mode]() {

    override def pickle(obj: Mode)(implicit state: PickleState): Unit = {
      import state.enc._
      obj match {
        case ContentInsertion(node, pos) =>
          writeInt(0)
          writeIntArray(node.toArray)
          writeInt(pos)
        case ContentNormal(node, range) =>
          writeInt(1)
          writeIntArray(node.toArray)
          IntRange.pickler.pickle(range)
        case ContentVisual(node, fix, move) =>
          writeInt(2)
          writeIntArray(node.toArray)
          writeInt(fix)
          writeInt(move)
        case NodeVisual(fix, move) =>
          writeInt(3)
          writeIntArray(fix.toArray)
          writeIntArray(move.toArray)
      }
    }

    override def unpickle(implicit state: UnpickleState): Mode = {
      import state.dec._
      readInt match {
        case 0 => ContentInsertion(readIntArray, readInt)
        case 1 => ContentNormal(readIntArray, IntRange.pickler.unpickle)
        case 2 => ContentVisual(readIntArray, readInt, readInt)
        case 3 => NodeVisual(readIntArray, readIntArray)
      }
    }
  }

}
