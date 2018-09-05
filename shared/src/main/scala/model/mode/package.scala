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
      def start: Int
      def focus: IntRange
      def fixed: IntRange = focus
      def merged: IntRange
    }
    sealed abstract class Code extends Content
    sealed trait NormalOrVisual extends Content
    sealed trait Normal extends NormalOrVisual

    case class RichInsert(pos: Int) extends Rich {
      override def breakWhiteSpaceInserts: Boolean = true
      override def end: Int = pos
      override def start: Int = pos

      override def focus: IntRange = IntRange(pos, pos)
      override def merged: IntRange = focus
    }

    sealed trait RichNormalOrVisual extends Rich {
      def copyWithNewFocus(r: IntRange): RichNormalOrVisual
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
      override def start: Int = range.start
    }

    object RichRange {
      def create(fix: IntRange, move: IntRange, enableModal: Boolean): RichRange = {
        if (enableModal) {
          RichVisual(fix, move)
        } else {
          if (fix.start <= move.start) {
            RichSelection(fix.start, move.until)
          } else {
            RichSelection(fix.until, move.start)
          }
        }
      }
    }
    sealed trait RichRange extends Rich {
      def merged: IntRange
      def leftIsAnchor: Boolean
    }

    case class RichSelection(fix: Int, move: Int) extends RichRange {
      assert(fix != move)

      override def leftIsAnchor: Boolean = fix < move

      override def merged: IntRange = if (fix < move) IntRange(fix, move) else IntRange(move, fix)

      override def end: Int = merged.until

      override def focus: IntRange = IntRange(move, move)
      def swap(leftIsAnchor: Boolean): RichSelection = if (leftIsAnchor) this else RichSelection(move, fix)

      override def start: Int = merged.start

    }
    case class RichVisual(fix: IntRange, move: IntRange) extends RichNormalOrVisual with RichRange {
      override def fixed: IntRange = fix

      override def leftIsAnchor: Boolean = fix.start <= move.start

      assert(fix.nonEmpty && move.nonEmpty && (fix == move || !fix.overlap(move)), s"wrong rich visual mode $fix $move")

      def collapse: Rich =
        model.mode.Content.RichNormal(move)

      def swap: RichVisual = RichVisual(move, fix)
      def swap(leftIsAnchor: Boolean): RichVisual = if (leftIsAnchor) this else swap
      def moveEnd = if (move.start > fix.start) move.until else move.start
      override def focus: IntRange = move
      override def merged: IntRange = fix.merge(move)
      override def copyWithNewFocus(range: IntRange): RichNormalOrVisual = copy(move = range)
      override def end: Int = merged.until
      override def start: Int = merged.start
    }

    sealed abstract class RichSubMode extends Rich {
      def getTextRange(rich: data.Rich): IntRange = rich.after(range.start - 1).textRange
      def getText(rich: data.Rich): Text.Delimited = rich.after(range.start - 1).text.asDelimited
      // range inside!
      def range: IntRange
      def modeBefore: Rich
      def copyWithRange(range: IntRange, rich: Rich): RichSubMode

      override def focus: IntRange = modeBefore.focus
      override def merged: IntRange = modeBefore.merged
    }

    // we assume that code will always have delimitation 1
    case class RichCodeSubMode(override val range: IntRange, code: CodeInside, override val modeBefore: Rich) extends RichSubMode {
      override def copyWithRange(range: IntRange, rich: Rich): RichSubMode = this.copy(range = range, modeBefore = rich)
      override def breakWhiteSpaceInserts: Boolean = code.breakWhiteSpaceInserts
      override def end: Int = modeBefore.end
      override def start: Int = modeBefore.start
    }

    case class RichAttributeSubMode(override val range: IntRange, override val modeBefore: Rich) extends RichSubMode {
      override def copyWithRange(range: IntRange, rich: Rich): RichSubMode = this.copy(range = range, modeBefore = rich)
      override def end: Int = modeBefore.end
      override def start: Int = modeBefore.start
    }

    case class CodeNormal(backToInsert: Boolean) extends Code with Normal //

    // insert, normal, visual, visual-line??
    case class CodeInside(mode: String, pos: Int) extends Code {
      override def breakWhiteSpaceInserts: Boolean = mode == "insert"
    }// user's mode is currently taken over by code editor

    object CodeInside {
      def empty(enableModal: Boolean) = CodeInside(if (enableModal) "normal" else "insert", 0)
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
          case Content(node, mode.Content.RichSelection(fix, move)) =>
            writeInt(3)
            writeIntArray(node.toArray)
            writeInt(fix)
            writeInt(move)
          case Visual(fix, move) =>
            writeInt(4)
            writeIntArray(fix.toArray)
            writeIntArray(move.toArray)
          case Content(node, mode.Content.CodeNormal(b)) =>
            writeInt(5)
            writeIntArray(node.toArray)
            writeByte(if (b) 1 else 0)
          case Content(node, mode.Content.CodeInside(a, b)) =>
            writeInt(6)
            writeIntArray(node.toArray)
            writeString(a)
            writeInt(b)
          case Content(node, mode.Content.RichCodeSubMode(range, code, mode)) =>
            writeInt(7)
            pickler.pickle(Content(node, mode))
            pickler.pickle(Content(node, code))
            IntRange.pickler.pickle(range)
          case Content(node, mode.Content.RichAttributeSubMode(range, mode)) =>
            writeInt(8)
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
          case 3 => Content(readIntArray, mode.Content.RichSelection(readInt, readInt))
          case 4 => Visual(readIntArray, readIntArray)
          case 5 => Content(readIntArray, mode.Content.CodeNormal(if (readByte == 0) false else true))
          case 6 => Content(readIntArray, mode.Content.CodeInside(readString, readInt))
          case 7 =>
            val r = unpickle(implicitly).asInstanceOf[mode.Node.Content]
            val j = unpickle(implicitly).asInstanceOf[mode.Node.Content].a.asInstanceOf[mode.Content.CodeInside]
            val ran = IntRange.pickler.unpickle
            Content(r.node, mode.Content.RichCodeSubMode(ran, j, r.a.asInstanceOf[mode.Content.Rich]))
          case 8 =>
            val r = unpickle(implicitly).asInstanceOf[mode.Node.Content]
            val ran = IntRange.pickler.unpickle
            Content(r.node, mode.Content.RichAttributeSubMode(ran, r.a.asInstanceOf[mode.Content.Rich]))
        }
      }
    }
  }
}
