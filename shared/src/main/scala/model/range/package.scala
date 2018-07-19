package model

import model.ot.Node.RebaseResult

package object range {


  case class IntRange(start: Int, until: Int) extends Iterable[Int] {


    override def toString(): String = s"($start, $until)"

    def merge(move: IntRange): IntRange = IntRange(start min move.start, until max move.until)


    def minusOrderedInside(except: Seq[IntRange]): Seq[IntRange] = {
      (Seq(start) ++ except.flatMap(a => Seq(a.start, a.until)) ++ Seq(until)).grouped(2).map(seq => IntRange(seq.head, seq(1))).filter(_.nonEmpty).toVector
    }

    override def iterator: Iterator[Int] = new Iterator[Int] {
      var i = start
      override def hasNext: Boolean = i < until

      override def next(): Int = {
        val r = i
        i += 1
        r
      }
    }

    def deletesCursor(i: Int): Boolean = start < i && until > i

    override def size: Int = until - start

    override def nonEmpty: Boolean = size > 0
    override def isEmpty: Boolean = size == 0

    def moveBy(a: Int): IntRange = IntRange(start + a, until + a)

    def moveByOrZeroZero(a: Int): IntRange =
      if (start + a >= 0) IntRange(start + a, until + a) else IntRange(0, 0)
    /**
      * positions: 0 1 2 3
      * range: [1, 2]
      * cursor: 0
      */
    def contains(i: Int): Boolean = i >= start && i < until


    def contains(b: IntRange): Boolean = b.start >= start && b.until <= until

    def overlap(b: IntRange): Boolean =
      contains(b.start) ||
        (b.size != 0 && contains(b.until - 1)) ||
          b.contains(start) ||
          (size != 0 && b.contains(until - 1))

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
      if (f.size == 0) {
        transformAfterDeleted(f.start).map(a => IntRange(a, a))
      } else {
        val l = transformAfterDeleted(f.start)
        val r = transformAfterDeleted(f.until - 1)
        (l, r) match {
          case (Some(ll), Some(rr)) => Some(IntRange(ll, rr + 1))
          case _ => None
        }
      }
    }

    def transformDeletingRangeAfterDeleted(f: IntRange): Option[IntRange] = {
      if (f.size == 0) {
        transformAfterDeleted(f.start).map(a => IntRange(a, a))
      } else {
        val l = transformAfterDeleted(f.start)
        val r = transformAfterDeleted(f.until - 1)
        (l, r) match {
          case (Some(ll), Some(rr)) => Some(IntRange(ll, rr + 1))
          case (Some(ll), None) => Some(IntRange(ll, start))
          case (None, Some(rr)) => Some(IntRange(start, rr + 1))
          case (None, None) =>  None
        }
      }
    }

  }


  object IntRange {

    def apply(a: Int): IntRange = IntRange(a, a + 1)

    val pickler: Pickler[IntRange] = new Pickler[IntRange] {
      override def pickle(obj: IntRange)(implicit state: PickleState): Unit = {
        import state.enc._
        writeInt(obj.start)
        writeInt(obj.until)
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
    childs: IntRange) extends Iterable[cursor.Node] {


    def transformNodeAfterMoved(to: cursor.Node, a: cursor.Node): cursor.Node = {
      if (contains(a)) {
        if (to.dropRight(1) == parent && to.last >= childs.until) {
          (to.dropRight(1) :+ (to.last + a(parent.size) - childs.until)) ++ a.drop(parent.size + 1)
        } else {
          (to.dropRight(1) :+ (to.last + a(parent.size) - childs.start)) ++ a.drop(parent.size + 1)
        }
      } else {
        cursor.Node.transformAfterInserted(transformAfterDeleted(to).get, childs.size, transformAfterDeleted(a).get)
      }
    }

    // move don't affect insert cursors before it, it is also inserted BEFORE the target insertion cursor
    def transformInsertionPointAfterMoved(to: cursor.Node, a: cursor.Node): cursor.Node = {
      ???
    }


    def start: cursor.Node = parent :+ childs.start
    def until: cursor.Node = parent :+ childs.until

    def transformDeletingRangeAfterDeleted(f: Node): Option[Node] = {
      if (sameParent(f.start)) {
        childs.transformDeletingRangeAfterDeleted(f.childs).map(a => Node(parent, a))
      } else {
        transformAfterDeleted(f.parent) match {
          case Some(a) => Some(f.copy(parent = a))
          case None => None
        }
      }
    }

    def transformAfterDeleted(at: cursor.Node): Option[cursor.Node] = {
      if (contains(at)) None
      else if (at.startsWith(parent) && at.size > parent.size && at(parent.size) >= childs.until) Some(at.patch(parent.size, Seq(at(parent.size) - childs.size), 1))
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

    override def iterator: Iterator[cursor.Node] = new Iterator[cursor.Node] {
      private val i = childs.iterator
      override def hasNext: Boolean = i.hasNext
      override def next(): cursor.Node = parent :+ i.next()
    }
  }

  object Node {

    def apply(t: cursor.Node, p: cursor.Node): Node = {
      assert(t.dropRight(1) == p.dropRight(1))
      Node(t.dropRight(1), IntRange(t.last, p.last + 1))
    }
    def apply(t: cursor.Node, p: IntRange): Node = {
      new Node(t, p)
    }

    def apply(t: cursor.Node, len: Int = 1): Node = {
      val last = t.last
      Node(t.dropRight(1), IntRange(last, last + len))
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
