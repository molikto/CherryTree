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

    // insert, normal, visual, visual-line??
    case class CodeInside(mode: String, pos: Int) extends Code // user's mode is currently taken over by code editor
  }

  sealed trait Node extends Mode[data.Node] {
    def inside(a: cursor.Node): Boolean
    def focus: cursor.Node
    def coverage: cursor.Node
  }

  object Node {

    case class Content(node: cursor.Node, a: mode.Content) extends Node {
      override def focus: cursor.Node = node
      override def coverage: cursor.Node = node
      def inside(zoom: cursor.Node): Boolean = cursor.Node.contains(zoom, node)
    }
    case class Visual(fix: cursor.Node, move: cursor.Node) extends Node {
      override def coverage: cursor.Node = minimalRange match {
        case Some(a) => a.parent
        case None => cursor.Node.root
      }

      def minimalRange: Option[range.Node] = cursor.Node.minimalRange(fix, move)
      def swap: Visual = Visual(move, fix)
      def inside(zoom: cursor.Node): Boolean = cursor.Node.contains(zoom, fix) && cursor.Node.contains(zoom, move)
      override def focus: cursor.Node = move
    }
  }
}
