package model

package object cursor {
  // when used as a insertion point, Seq.empty is generally invalid, because the app will not allow deleting root
  type Node = Seq[Int]

  object IntSeq {
    def transformInsertionPointAfterInserted(inserted: Int, len: Int, ref: Int): Int = {
      if (ref < inserted) {
        ref
      } else { // so the case of asymmetry is here
        ref + len
      }
    }
  }


  object Node {
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
    def transformInsertionPointAfterInserted(inserted: Node, len: Int, ref: Node): Node = {
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
          case _ => throw new IllegalStateException("Not possible")
        }
      }
    }
  }
}
