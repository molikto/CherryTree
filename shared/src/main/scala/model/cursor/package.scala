package model

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

    class Mover(root: data.Node, isClosed: Node => Boolean) {

      def parent(a: Node): Option[Node] = {
        if (a.isEmpty) None else Some(a.dropRight(1))
      }

      def firstChild(a: Node): Option[Node] = {
        val size = root(a).childs.size
        if (size > 0) {
          Some(a :+ 0)
        } else {
          None
        }
      }
      def lastChild(a: Node): Option[Node] = {
        val size = root(a).childs.size
        if (size == 0) {
          None
        } else {
          Some(a :+ (size - 1))
        }
      }
      def previous(a: Node): Option[Node] = {
        if (a.isEmpty) {
          None
        } else {
          val last = a.last
          if (last > 0) {
            Some(a.dropRight(1) :+ (last - 1))
          } else {
            None
          }
        }
      }

      def nextOver(a: Node): Node = {
        if (a.isEmpty) {
          throw new IllegalArgumentException("Root has no next")
        } else {
          a.dropRight(1) :+ (a.last + 1)
        }
      }

      def next(a: Node): Option[Node] = {
        if (a.isEmpty) {
          None
        } else {
          val last = a.last
          if (last < root(a.dropRight(1)).childs.size) {
            Some(a.dropRight(1) :+ (last + 1))
          } else {
            None
          }
        }
      }

      def visualBottom(k: Node): Node = {
        if (isClosed(k)) {
          k
        } else {
          lastChild(k) match {
            case Some(a) => visualBottom(a)
            case _ => k
          }
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
        if (!isClosed(a)) {
          firstChild(a).orElse(globalNext(a))
        } else {
          globalNext(a)
        }
      }
    }




    val root: Node = Seq.empty
    /**
      * @return common, left unique, right unique
      */
    def destructRelative(a: Node, b: Node): (Seq[Int], Seq[Int], Seq[Int]) = {
      val len = a.size min b.size
      val common = (0 until len).takeWhile(i => a(i) == b(i)).lastOption.getOrElse(-1)
      if (common == -1) {
        (scala.Seq.empty, a, b)
      } else {
        val i = common + 1
        (a.take(i), a.drop(i), b.drop(i))
      }
    }

    /**
      * @param inserted the previous insertion point
      * @param len the previous insertion point length
      * @param ref the current insertion point (not rebased)
      * @return the rebased current insertion point
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
