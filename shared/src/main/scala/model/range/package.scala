package model

package object range {


  case class IntRange(start: Int, endInclusive: Int) {

    def size: Int = endInclusive - start + 1

    def until: Int = endInclusive + 1

    def assertSize(): Unit = {
      assert(start >= 0 && endInclusive >= start)
    }

    def moveBy(a: Int): IntRange = IntRange(start + a, endInclusive + a)

    /**
      * positions: 0 1 2 3
      * range: [1, 2]
      * cursor: 0
      */
    def contains(cursor: Int): Boolean = cursor >= start && cursor <= endInclusive


    def contains(b: IntRange): Boolean = b.start >= start && b.endInclusive <= endInclusive

    def overlap(b: IntRange): Boolean =
      contains(b.start) || contains(b.endInclusive) || b.contains(start) || b.contains(b.endInclusive)

    def transformAfterDeleted(p: Int): Option[Int] = {
      if (p < start) {
        Some(p)
      } else if (contains(p)) {
        None
      } else {
        Some(p - size)
      }
    }

    /**
      * @return None if either side of `s` is deleted
      */
    def transformAfterDeleted(f: IntRange): Option[IntRange] = {
      val l = transformAfterDeleted(f.start)
      val r = transformAfterDeleted(f.endInclusive)
      (l, r) match {
        case (Some(ll), Some(rr)) => Some(IntRange(ll, rr))
        case _ => None
      }
    }

    def transformDeletingRangeAfterDeleted(f: IntRange): Option[IntRange] = {
      val l = transformAfterDeleted(f.start)
      val r = transformAfterDeleted(f.endInclusive)
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

    def start: cursor.Node = parent :+ childs.start
    def endInclusive: cursor.Node = parent :+ childs.endInclusive

    def transformDeletingRangeAfterDeleted(f: Node): Option[Node] = {
      if (sameParent(f.start)) {
        childs.transformDeletingRangeAfterDeleted(f.childs).map(a => Node(parent, a))
      } else {
        val l = transformAfterDeleted(f.start)
        val r = transformAfterDeleted(f.endInclusive)
        (l, r) match {
          case (Some(ll), Some(rr)) => Some(Node(ll, rr))
          case (None, None) =>  None
          case _ => throw new IllegalStateException()
        }
      }
    }

    def transformAfterDeleted(at: cursor.Node): Option[cursor.Node] = {
      if (contains(at)) None
      else if (at.startsWith(parent) && at(parent.size) > childs.endInclusive) Some(at.patch(parent.size, Seq(at(parent.size + childs.size)), 1))
      else Some(at)
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
    def sameParent(at: cursor.Node) = at.size == parent.size + 1 && at.startsWith(parent)


    def contains(at: cursor.Node): Boolean = at.size > parent.size && at.startsWith(parent) && childs.contains(at(parent.size))
  }

  object Node {

    def apply(t: cursor.Node, p: cursor.Node): Node = {
      assert(t.dropRight(1) == p.dropRight(1))
      Node(t.dropRight(1), IntRange(t.last, p.last))
    }
    def apply(t: cursor.Node, p: IntRange): Node = {
      new Node(t, p)
    }

    def apply(t: cursor.Node, len: Int = 1): Node = {
      val last = t.last
      Node(t.dropRight(1), IntRange(last, last + len - 1))
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
