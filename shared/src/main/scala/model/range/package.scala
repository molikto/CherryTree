package model

import boopickle.{PickleState, Pickler, UnpickleState}

package object range {


  trait IntRange {
    val start: Int
    val endInclusive: Int

    def size: Int = endInclusive - start + 1

    def until: Int = endInclusive + 1

    def assertSize(): Unit = {
      assert(start >= 0 && endInclusive >= start)
    }
    def contains(cursor: Int): Boolean = cursor > start && cursor < endInclusive
    def touch(cursor: Int): Boolean = cursor >= start && cursor <= endInclusive




    def transformCursorAfterDeleted(p: Int): Option[Int] = {
      if (p < start) {
        Some(p)
      } else if (contains(p)) {
        None
      } else {
        Some(p - size)
      }
    }

    def transformInsertionPointAfterDeleted(p: Int): Int = {
      if (p < start) {
        p
      } else if (contains(p)) {
        start
      } else {
        p - size
      }
    }

    /**
      * @return None if either side of `s` is deleted
      */
    def transformAfterDeleted(f: range.IntRange): Option[range.IntRange] = {
      val l = transformCursorAfterDeleted(f.start)
      val r = transformCursorAfterDeleted(f.endInclusive)
      (l, r) match {
        case (Some(ll), Some(rr)) => Some(range.IntRange(ll, rr))
        case _ => None
      }
    }

    def transformDeletingRangeAfterDeleted(f: range.IntRange): Option[range.IntRange] = {
      val l = transformCursorAfterDeleted(f.start)
      val r = transformCursorAfterDeleted(f.endInclusive)
      (l, r) match {
        case (Some(ll), Some(rr)) => Some(range.IntRange(ll, rr))
        case (Some(ll), None) => Some(range.IntRange(ll, start - 1))
        case (None, Some(rr)) => Some(range.IntRange(endInclusive, rr))
        case (None, None) =>  None
      }
    }
  }

  case class DefaultIntRange(override val start: Int, override val endInclusive: Int) extends IntRange

  object IntRange {
    def apply(start: Int, end: Int) = DefaultIntRange(start, end)

    val pickler: Pickler[IntRange] = new Pickler[IntRange] {
      override def pickle(obj: IntRange)(implicit state: PickleState): Unit = {
        import state.enc._
        writeInt(obj.start)
        writeInt(obj.endInclusive)
      }

      override def unpickle(implicit state: UnpickleState): IntRange = {
        import state.dec._
        IntRange(readInt, readInt)
      }
    }
  }


  type Unicode = IntRange

  /**
    * unable to represent a range of the node itself, our app doesn't has this functionality
    */
  case class Node(parent: cursor.Node,
    override val start: Int,
    override val endInclusive: Int) extends IntRange {
    def transformInsertionPointAfterDeleted(at: cursor.Node): cursor.Node =
      if (contains(at)) parent :+ start
      else if (at.startsWith(parent) && at(parent.size) > endInclusive) parent ++ Seq(at(parent.size) - 1) ++ at.drop(parent.size + 1)
      else at

    def contains(at: cursor.Node): Boolean = at.size > parent.size && at.startsWith(parent) && contains(at(parent.size))
  }

  object Node {
    val pickler: Pickler[Node] = new Pickler[Node] {

      override def pickle(obj: Node)(implicit state: PickleState): Unit = {
        import state.enc._
        writeIntArray(obj.parent)
        writeInt(obj.start)
        writeInt(obj.endInclusive)
      }

      override def unpickle(implicit state: UnpickleState): Node = {
        import state.dec._
        Node(readIntArray(), readInt, readInt)
      }
    }
  }
}
