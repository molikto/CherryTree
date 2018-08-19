package model

import model.range.IntRange

package object cursor {
  // when used as a insertion point, Seq.empty is generally invalid, because the app will not allow deleting root

  object IntSeq {
    def transformAfterInserted(inserted: Int, len: Int, ref: Int): Int = {
      if (ref < inserted) {
        ref
      } else { // so the case of asymmetry is here
        ref + len
      }
    }
  }

  type Node = Seq[Int]


  object Node {
    def parent(a: Node) = a.dropRight(1)
    def contains(zoom: Node, node: Node): Boolean = node.startsWith(zoom)


    def moveBy(a: cursor.Node, i: Int): cursor.Node = model.cursor.Node.parent(a) :+ (a.last + i)

    class Mover(root: data.Node, zoom: cursor.Node, isFolded: Node => Boolean) {



      def parent(a: Node): Option[Node] = {
        if (a == zoom) None else Some(model.cursor.Node.parent(a))
      }

      def size(a: Node): Int = root(a).childs.size

      def firstChild(a: Node): Option[Node] = {
        if (isFolded(a)) None
        else {
          val size = root(a).childs.size
          if (size > 0) {
            Some(a :+ 0)
          } else {
            None
          }
        }
      }
      def lastChild(a: Node): Option[Node] = {
        if (isFolded(a)) None
        else {
          val size = root(a).childs.size
          if (size == 0) {
            None
          } else {
            Some(a :+ (size - 1))
          }
        }
      }

      def previous(a: Node): Option[Node] = {
        if (a == zoom) {
          None
        } else {
          val last = a.last
          if (last > 0) {
            Some(model.cursor.Node.parent(a) :+ (last - 1))
          } else {
            None
          }
        }
      }

      def nextOver(a: Node): Node = {
        if (a == zoom) {
          throw new IllegalArgumentException("Root has no next")
        } else {
          model.cursor.Node.parent(a) :+ (a.last + 1)
        }
      }

      def next(a: Node): Option[Node] = {
        if (a == zoom) {
          None
        } else {
          val last = a.last
          if (last < root(model.cursor.Node.parent(a)).childs.size - 1) {
            Some(model.cursor.Node.parent(a) :+ (last + 1))
          } else {
            None
          }
        }
      }

      def visualBottom(k: Node): Node = {
        lastChild(k) match {
          case Some(a) => visualBottom(a)
          case _ => k
        }
      }

      def visualUp(a: Node): Option[Node] = {
        previous(a) match {
          case None => parent(a)
          case Some(k) => Some(visualBottom(k))
        }
      }


      def globalNext(a: Node): Option[Node] = next(a).orElse(parent(a).flatMap(globalNext))

      def visualDown(a: Node): Option[Node] = {
        firstChild(a).orElse(globalNext(a))
      }
    }




    val root: Node = Seq.empty
    /**
      * @return common, left unique, right unique
      */
    private def destructRelative(a: Node, b: Node): (Seq[Int], Seq[Int], Seq[Int]) = {
      val len = a.size min b.size
      val common = (0 until len).takeWhile(i => a(i) == b(i)).lastOption.getOrElse(-1)
      if (common == -1) {
        (scala.Seq.empty, a, b)
      } else {
        val i = common + 1
        (a.take(i), a.drop(i), b.drop(i))
      }
    }

    def minimalRange(a: Node, b: Node): Option[range.Node]  = {
      val (common, lu, ru) = destructRelative(a, b)
      if (lu.isEmpty || ru.isEmpty) {
        if (common.isEmpty) {
          None
        } else {
          Some(range.Node(common))
        }
      } else {
        Some(range.Node(common,
          if (lu.head < ru.head) IntRange(lu.head, ru.head + 1) else IntRange(ru.head, lu.head + 1)))
      }
    }

    /**
      * @param inserted the previous insertion point
      * @param len the previous insertion point length
      * @param ref the pos
      * @return the rebased pos
      */
    def transformAfterInserted(inserted: Node, len: Int, ref: Node): Node = {
      val (common, ii, rr) = destructRelative(inserted, ref)
      // if rr.size < ii.size, the the current insertion happens higher, so not affected by previous one
      // else rr.size >= ii.size
      // else if ii.size > 1, then rr.size >= 1, then they split at some point without affecting each
      // so ii <= 1 && rr.size >= ii.size
      if (ii.size > 1 || rr.size < ii.size) {
        ref
      } else {
        (ii.headOption, rr.headOption) match {
          // if ii == 1, then rr >= 1
          case (Some(i), Some(r)) =>
            if (r < i) {
              ref
            } else {
              // in the case rr > 1, it is a cursor, and it moves after insertion
              // in the case of rr == 1 then it is a insertion point, we do the same uniformly as other asymmetry cases
              ref.patch(common.size, Seq(r + len), 1)
            }
          // if ii == 0, then the insertion cursor is moved by len
          case (None, _) =>
            ref.patch(common.size - 1, Seq(ref(common.size - 1) + len), 1)
          case _ => throw new IllegalStateException("Not possible case")
        }
      }
    }
  }
}
