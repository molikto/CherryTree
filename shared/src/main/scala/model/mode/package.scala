package model

import model.range.IntRange

package object mode {
  trait Mode[T] {
  }

  sealed abstract class Content extends Mode[data.Content] {
    def isNormalOrVisual: Boolean = this.isInstanceOf[Content.NormalOrVisual]
    def isNormal: Boolean = this.isInstanceOf[Content.Normal]
  }

  object Content {
    sealed abstract class Rich extends Content
    sealed abstract class Code extends Content
    sealed trait NormalOrVisual extends Content
    sealed trait Normal extends NormalOrVisual

    sealed abstract class RichNormalOrVisual extends Rich with NormalOrVisual {
      def copyWithNewFocus(range: IntRange): RichNormalOrVisual
      def focus: IntRange
      def merged: IntRange
    }
    case class RichInsert(pos: Int) extends Rich {
    }
    /**
      * second parameter is a range because selection is not just one codepoint
      *
      * press copy in visual mode you get different result in different editors
      * we adapt one...
      *
      * empty selection is only valid when document is empty
      */
    case class RichNormal(range: IntRange) extends RichNormalOrVisual with Normal {
      assert(range.size != 0 || range.start == 0) // try to avoid empty selection error
      def isEmpty: Boolean = range.isEmpty

      override def focus: IntRange = range
      override def merged: IntRange = range
      override def copyWithNewFocus(r: IntRange): RichNormalOrVisual = copy(range = r)
    }
    case class RichVisual(fix: IntRange, move: IntRange) extends RichNormalOrVisual {
      def swap: RichVisual = RichVisual(move, fix)
      override def focus: IntRange = move
      override def merged: IntRange = fix.merge(move)
      override def copyWithNewFocus(range: IntRange): RichNormalOrVisual = copy(move = range)
    }


    case object CodeNormal extends Code with Normal //

    case object CodeInside extends Code // user's mode is currently taken over by code editor
  }

  sealed trait Node extends Mode[data.Node] {
    def inside(a: cursor.Node): Boolean
    def focus: cursor.Node
  }

  object Node {

    case class Content(node: cursor.Node, a: mode.Content) extends Node {
      override def focus: cursor.Node = node
      def inside(zoom: cursor.Node): Boolean = cursor.Node.contains(zoom, node)
    }
    case class Visual(fix: cursor.Node, move: cursor.Node) extends Node {
      def minimalRange: Option[range.Node] = cursor.Node.minimalRange(fix, move)
      def swap: Visual = Visual(move, fix)
      def inside(zoom: cursor.Node): Boolean = cursor.Node.contains(zoom, fix) && cursor.Node.contains(zoom, move)
      override def focus: cursor.Node = move
    }


    val pickler: Pickler[Node] = new Pickler[Node]() {

      override def pickle(obj: Node)(implicit state: PickleState): Unit = {
        import state.enc._
        obj match {
          case Content(node, mode.Content.RichInsert(pos)) =>
            writeInt(0)
            writeIntArray(node.toArray)
            writeInt(pos)
          case Content(node, mode.Content.RichNormal(range)) =>
            writeInt(1)
            writeIntArray(node.toArray)
            IntRange.pickler.pickle(range)
          case Content(node, mode.Content.RichVisual(fix, move)) =>
            writeInt(2)
            writeIntArray(node.toArray)
            IntRange.pickler.pickle(fix)
            IntRange.pickler.pickle(move)
          case Visual(fix, move) =>
            writeInt(3)
            writeIntArray(fix.toArray)
            writeIntArray(move.toArray)
          case Content(node, mode.Content.CodeNormal) =>
            writeInt(4)
            writeIntArray(node.toArray)
          case Content(node, mode.Content.CodeInside) =>
            writeInt(5)
            writeIntArray(node.toArray)
        }
      }

      override def unpickle(implicit state: UnpickleState): Node = {
        import state.dec._
        readInt match {
          case 0 => Content(readIntArray, mode.Content.RichInsert(readInt))
          case 1 => Content(readIntArray, mode.Content.RichNormal(IntRange.pickler.unpickle))
          case 2 => Content(readIntArray, mode.Content.RichVisual(IntRange.pickler.unpickle, IntRange.pickler.unpickle))
          case 3 => Visual(readIntArray, readIntArray)
          case 4 => Content(readIntArray, mode.Content.CodeNormal)
          case 5 => Content(readIntArray, mode.Content.CodeInside)
        }
      }
    }
  }
}
