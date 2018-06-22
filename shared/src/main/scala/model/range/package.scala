package model

import boopickle.{PickleState, Pickler, UnpickleState}

package object range {


  case class IntRange(start: Int, endInclusive: Int) {

    def size: Int = endInclusive - start + 1

    def until: Int = endInclusive + 1

    def assertSize(): Unit = {
      assert(start >= 0 && endInclusive >= start)
    }

    /**
      * positions: 0 1 2 3
      * range: [1, 2]
      * cursor: 0
      */
    def containsCursor(cursor: Int): Boolean = cursor >= start && cursor <= endInclusive

    def containsInsertionPoint(cursor: Int): Boolean = cursor > start && cursor <= endInclusive

    def touchesInsertionPoint(cursor: Int): Boolean = cursor >= start && cursor <= endInclusive + 1



    def transformCursorAfterDeleted(p: Int): Option[Int] = {
      if (p < start) {
        Some(p)
      } else if (containsCursor(p)) {
        None
      } else {
        Some(p - size)
      }
    }

    def transformInsertionPointAfterDeleted(p: Int): Int = {
      if (p < start) {
        p
      } else if (touchesInsertionPoint(p)) {
        start
      } else {
        p - size
      }
    }

    /**
      * @return None if either side of `s` is deleted
      */
    def transformAfterDeleted(f: IntRange): Option[IntRange] = {
      val l = transformCursorAfterDeleted(f.start)
      val r = transformCursorAfterDeleted(f.endInclusive)
      (l, r) match {
        case (Some(ll), Some(rr)) => Some(IntRange(ll, rr))
        case _ => None
      }
    }

    def transformDeletingRangeAfterDeleted(f: IntRange): Option[IntRange] = {
      val l = transformCursorAfterDeleted(f.start)
      val r = transformCursorAfterDeleted(f.endInclusive)
      (l, r) match {
        case (Some(ll), Some(rr)) => Some(IntRange(ll, rr))
        case (Some(ll), None) => Some(IntRange(ll, start - 1))
        case (None, Some(rr)) => Some(IntRange(endInclusive, rr))
        case (None, None) =>  None
      }
    }
  }


  object IntRange {

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


  /**
    * unable to represent a range of the node itself, our app doesn't has this functionality
    */
  case class Node(parent: cursor.Node,
    childs: IntRange) {
    def transformInsertionPointAfterDeleted(at: cursor.Node): cursor.Node =
      if (containsInsertionPoint(at)) parent :+ childs.start
      else if (at.startsWith(parent) && at(parent.size) > childs.endInclusive) parent ++ Seq(at(parent.size) - 1) ++ at.drop(parent.size + 1)
      else at


    def transformCursorAfterDeleted(at: cursor.Node): cursor.Node = {
      ???
    }
    /**
      * same parent, same level
      * 111
      * 11[3, 4]
      *
      *
      * same parent, different level
      * 1111
      * 11[3, 4]
      */
    private def sameParent(at: cursor.Node) = at.size > parent.size && at.startsWith(parent)

    def sameLevel(at: cursor.Node): Boolean = at.size == parent.size + 1

    def containsInsertionPoint(at: cursor.Node): Boolean = {
      if (sameParent(at)) {
        if (sameLevel(at)) {
          childs.containsInsertionPoint(at.last)
        } else {
          childs.containsCursor(at(parent.size))
        }
      } else {
        false
      }
    }
    def touchesInsertionPoint(at: cursor.Node): Boolean = {
      if (sameParent(at)) {
        if (sameLevel(at)) {
          childs.touchesInsertionPoint(at.last)
        } else {
          childs.containsCursor(at(parent.size))
        }
      } else {
        false
      }
    }

    def containsCursor(at: cursor.Node): Boolean = at.size > parent.size && at.startsWith(parent) && childs.containsCursor(at(parent.size))
  }

  object Node {

    def apply(t: cursor.Node): Node = {
      val last = t.last
      Node(t.dropRight(1), IntRange(last, last))
    }

    val pickler: Pickler[Node] = new Pickler[Node] {

      override def pickle(obj: Node)(implicit state: PickleState): Unit = {
        import state.enc._
        writeIntArray(obj.parent.toArray)
        IntRange.pickler.pickle(obj.childs)
      }

      override def unpickle(implicit state: UnpickleState): Node = {
        import state.dec._
        Node(readIntArray(), IntRange.pickler.unpickle)
      }
    }
  }
}
