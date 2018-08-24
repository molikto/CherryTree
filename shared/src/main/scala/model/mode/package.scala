package model

import model.data.Text
import model.range.IntRange

package object mode {
  trait Mode[T] {
    def breakWhiteSpaceInserts: Boolean = false
  }

  sealed abstract class Content extends Mode[data.Content] {
    def isNormalOrVisual: Boolean = this.isInstanceOf[Content.NormalOrVisual]
    def isNormal: Boolean = this.isInstanceOf[Content.Normal]
  }

  object Content {
    sealed abstract class Rich extends Content {
      def end: Int
    }
    sealed abstract class Code extends Content
    sealed trait NormalOrVisual extends Content
    sealed trait Normal extends NormalOrVisual

    sealed abstract class RichNormalOrVisual extends Rich with NormalOrVisual {
      def copyWithNewFocus(range: IntRange): RichNormalOrVisual
      def focus: IntRange
      def merged: IntRange
    }
    case class RichInsert(pos: Int) extends Rich {
      override def breakWhiteSpaceInserts: Boolean = true
      override def end: Int = pos
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

      def collapse(enableModal: Boolean): Rich = if (enableModal)  this else RichInsert(range.until)

      override def focus: IntRange = range
      override def merged: IntRange = range
      override def copyWithNewFocus(r: IntRange): RichNormalOrVisual = copy(range = r)
      override def end: Int = range.until
    }
    case class RichVisual(fix: IntRange, move: IntRange) extends RichNormalOrVisual {
      def collapse(enableModal: Boolean): Rich = if (enableModal) model.mode.Content.RichNormal(move) else model.mode.Content.RichInsert(moveEnd)

      def swap: RichVisual = RichVisual(move, fix)
      def moveEnd = if (move.start > fix.start) move.until else move.start
      override def focus: IntRange = move
      override def merged: IntRange = fix.merge(move)
      override def copyWithNewFocus(range: IntRange): RichNormalOrVisual = copy(move = range)
      override def end: Int = merged.until
    }

    sealed abstract class RichSubMode extends Rich {
      def getTextRange(rich: data.Rich): IntRange = rich.after(range.start - 1).textRange
      def getText(rich: data.Rich): Text.Delimited = rich.after(range.start - 1).text.asDelimited
      // range inside!
      def range: IntRange
      def modeBefore: Rich
      def copyWithRange(range: IntRange, rich: Rich): RichSubMode

    }

    // we assume that code will always have delimitation 1
    case class RichCodeSubMode(override val range: IntRange, code: CodeInside, override val modeBefore: Rich) extends RichSubMode {
      override def copyWithRange(range: IntRange, rich: Rich): RichSubMode = this.copy(range = range, modeBefore = rich)
      override def breakWhiteSpaceInserts: Boolean = code.breakWhiteSpaceInserts
      override def end: Int = modeBefore.end
    }

    case class RichAttributeSubMode(override val range: IntRange, override val modeBefore: Rich) extends RichSubMode {
      override def copyWithRange(range: IntRange, rich: Rich): RichSubMode = this.copy(range = range, modeBefore = rich)
      override def end: Int = modeBefore.end
    }

    case object CodeNormal extends Code with Normal //

    // insert, normal, visual, visual-line??
    case class CodeInside(mode: String, pos: Int) extends Code {
      override def breakWhiteSpaceInserts: Boolean = mode == "insert"
    }// user's mode is currently taken over by code editor

    object CodeInside {
      val empty = CodeInside("normal", 0)
    }
  }

  sealed trait Node extends Mode[data.Node] {

    def inside(a: cursor.Node): Boolean
    def focus: cursor.Node
    def other: cursor.Node = focus
    def coverage: cursor.Node
  }

  object Node {

    case class Content(node: cursor.Node, a: mode.Content) extends Node {
      override def focus: cursor.Node = node
      override def coverage: cursor.Node = node
      def inside(zoom: cursor.Node): Boolean = cursor.Node.contains(zoom, node)
      override def breakWhiteSpaceInserts: Boolean = a.breakWhiteSpaceInserts
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
      override def other: cursor.Node = fix
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
          case Content(node, mode.Content.CodeInside(a, b)) =>
            writeInt(5)
            writeIntArray(node.toArray)
            writeString(a)
            writeInt(b)
          case Content(node, mode.Content.RichCodeSubMode(range, code, mode)) =>
            writeInt(6)
            pickler.pickle(Content(node, mode))
            pickler.pickle(Content(node, code))
            IntRange.pickler.pickle(range)
          case Content(node, mode.Content.RichAttributeSubMode(range, mode)) =>
            writeInt(7)
            pickler.pickle(Content(node, mode))
            IntRange.pickler.pickle(range)
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
          case 5 => Content(readIntArray, mode.Content.CodeInside(readString, readInt))
          case 6 =>
            val r = unpickle(implicitly).asInstanceOf[mode.Node.Content]
            val j = unpickle(implicitly).asInstanceOf[mode.Node.Content].a.asInstanceOf[mode.Content.CodeInside]
            val ran = IntRange.pickler.unpickle
            Content(r.node, mode.Content.RichCodeSubMode(ran, j, r.a.asInstanceOf[mode.Content.Rich]))
          case 7 =>
            val r = unpickle(implicitly).asInstanceOf[mode.Node.Content]
            val ran = IntRange.pickler.unpickle
            Content(r.node, mode.Content.RichAttributeSubMode(ran, r.a.asInstanceOf[mode.Content.Rich]))
        }
      }
    }
  }
}
