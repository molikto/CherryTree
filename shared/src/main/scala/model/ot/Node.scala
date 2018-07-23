package model.ot


import boopickle.Pickler
import model._
import com.softwaremill.quicklens._
import model.range.IntRange

import scala.util.Random

object Node extends Ot[data.Node, operation.Node, conflict.Node] {

  type RebaseResult = Rebased[conflict.Node, (Seq[operation.Node], Seq[operation.Node])]



  // LATER handle move
  override def rebase(winner: operation.Node, loser: operation.Node): RebaseResult = {

    def moveBy(a: cursor.Node, i: Int): cursor.Node = a.dropRight(1) :+ (a.last + i)

    def insertMove(i: operation.Node.Insert, d: operation.Node.Move): RebaseResult = {
      // if insertion point is the same as move start point, insertion point is not moved
      def transformByI(a: cursor.Node) = cursor.Node.transformAfterInserted(i.at, i.childs.size, a)
      def transformUntilByI() =
        if (d.r.isEmpty) transformByI(d.r.until)
        else moveBy(transformByI(moveBy(d.r.until, -1)), 1)
      free(
        i.copy(at = d.r.transformInsertionPointAfterMoved(d.to, i.at)),
        operation.Node.Move(
          range.Node(transformByI(d.r.start), transformUntilByI()),
          if (i.at == d.to) d.to else transformByI(d.to))) // moved to always stays at top of insert to
    }


    def ignoreNullMoves(a: range.Node, b: cursor.Node): Seq[operation.Node.Move] = {
      if (a.contains(b) || a.until == b) {
        Seq.empty
      } else {
        Seq(operation.Node.Move(a, b))
      }
    }

    def reverseMove(m: operation.Node.Move): operation.Node.Move = {
      operation.Node.Move(
        range.Node(moveBy(m.r.transformNodeAfterMoved(m.to, m.to), -m.r.size), m.r.size),
        m.r.transformNodeAfterMoved(m.to, m.r.until)
      )
    }
    def deleteMove(w: operation.Node.Delete, m: operation.Node.Move, deleteConflict : => conflict.Node): RebaseResult = {
      if (w.r.contains(m.to.dropRight(1))) { // move should be entirely deleted
        val deletedAfterMoved = m.r.split(w.r).map(dd => range.Node(m.r.transformNodeAfterMoved(m.to, dd.start), dd.size)) ++
          (if (w.r.contains(m.r.parent)) Seq(range.Node(m.r.transformNodeAfterMoved(m.to, m.r.start), m.r.size)) else Seq.empty )
        val mdAfterMoved = range.Node(m.r.transformNodeAfterMoved(m.to, m.r.start), m.r.size)
        free(operation.Node.deleteRanges(deletedAfterMoved :+ mdAfterMoved), w.r.transformDeletingRangeAfterDeleted(m.r).map(a => operation.Node.Delete(a)).toSeq)
      } else {
        val moveTo = w.r.transformAfterDeleted(m.to).getOrElse(w.r.start)
        val mtoS = range.Node(m.to.dropRight(1), IntRange(0, m.to.last))
        val deletedAfterMoved = m.r.split(w.r).flatMap(a => mtoS.split(a)).map(dd => range.Node(m.r.transformNodeAfterMoved(m.to, dd.start), dd.size)) ++
          (if (w.r.contains(m.r.parent)) Seq(range.Node(m.r.transformNodeAfterMoved(m.to, m.r.start), m.r.size)) else Seq.empty )
        val delete = operation.Node.deleteRanges(deletedAfterMoved)
        val move = w.r.transformDeletingRangeAfterDeleted(m.r) match {
          case Some(moveRange) => // there is still some range can do the move
            ignoreNullMoves(moveRange, moveTo)
          case None => // delete deletes entire moved range
            Seq.empty
        }
        free(delete, move)
      }
    }

    def insertDelete(i: operation.Node.Insert, d: operation.Node.Delete, deleteConflict : => conflict.Node): RebaseResult = {
      if (d.r.sameParent(i.at)) {
        val at = i.at.last
        val left = d.r.childs.start
        val right = d.r.childs.until - 1
        if (at <= left) {
          free(i, d.modify(_.r).using(_.modify(_.childs).using(_.moveBy(i.childs.size))))
        } else if (at > left && at <= right) {
           //[][]
          // [].....[]
          val range1 = IntRange(left, at)
          val end = right + i.childs.size
          // end.size = d.r.childs.size - range1.size = right - left + 1
          val range2 = IntRange(end + 1 - (d.r.childs.size - range1.size), end + 1)
          free(
            Seq(i.modify(_.at).using(a => a.dropRight(1) :+ left)),
            Seq(
              operation.Node.Delete(d.r.copy(childs = range2)),
              operation.Node.Delete(d.r.copy(childs = range1))
          ))
        } else {
          free(i.modify(_.at).using(a => a.dropRight(1) :+ (a.last - d.r.childs.size)), d)
        }
      } else {
        d.r.transformAfterDeleted(i.at) match {
          case Some(p) =>
            free(i.copy(at = p), d.modify(_.r).using(r => range.Node(cursor.Node.transformAfterInserted(i.at, i.childs.size, r.start), r.childs.size)))
          case None =>
            Rebased(Set(deleteConflict), (Seq.empty, Seq(d)))
        }
      }
    }
    def reverse(res: RebaseResult) = Rebased(res.conflicts, (res.t._2, res.t._1))
    winner match {
      case w@operation.Node.Content(wc, wo) =>
        loser match {
          case operation.Node.Content(lc, lo) =>
            if (wc == lc) {
              val r = Content.rebase(wo, lo)
              Rebased(r.conflicts.map(c => conflict.Node.Content(c)), map[operation.Content, operation.Node](r.t, a => operation.Node.Content(wc, a)))
            } else {
              free(winner, loser)
            }
          case operation.Node.Replace(lc, _) =>
            if (wc == lc) {
              Rebased(Set(conflict.Node.ReplacedByLoser()), (Seq.empty, Seq(loser)))
            } else {
              free(winner, loser)
            }
          case operation.Node.Insert(lc, lcs) =>
            free(w.copy(at = cursor.Node.transformAfterInserted(lc, lcs.size, wc)), loser)
          case operation.Node.Delete(lr) =>
            lr.transformAfterDeleted(wc) match {
              case Some(p) => free(w.copy(at = p), loser)
              case None => Rebased(Set(conflict.Node.LoserDeletesWinner()), (Seq.empty, Seq(loser)))
            }
          case m@operation.Node.Move(lr, la) =>
            free(w.copy(at = lr.transformNodeAfterMoved(la, wc)), m)
        }
      case w@operation.Node.Replace(wc, _) =>
        loser match {
          case operation.Node.Content(lc, _) =>
            if (wc == lc) {
              Rebased(Set(conflict.Node.ReplacedByWinner()), (Seq(winner), Seq.empty))
            } else {
              free(winner, loser)
            }
          case operation.Node.Replace(lc, _) =>
            if (wc == lc) {
              Rebased(Set(conflict.Node.WinnerDeletesLoser()), (Seq(winner), Seq.empty))
            } else {
              free(winner, loser)
            }
          case operation.Node.Insert(lc, lcs) =>
            free(w.copy(at = cursor.Node.transformAfterInserted(lc, lcs.size, wc)), loser)
          case operation.Node.Delete(lr) =>
            lr.transformAfterDeleted(wc) match {
              case Some(p) => free(w.copy(at = p), loser)
              case None => Rebased(Set(conflict.Node.LoserDeletesWinner()), (Seq.empty, Seq(loser)))
            }
          case m@operation.Node.Move(lr, la) =>
            free(w.copy(at = lr.transformNodeAfterMoved(la, wc)), m)
        }
      case w@operation.Node.Insert(wc, wcs) =>
        loser match {
          case l@operation.Node.Content(lc, _) =>
            free(w, l.copy(at = cursor.Node.transformAfterInserted(wc, wcs.size, lc)))
          case l@operation.Node.Replace(lc, _) =>
            free(w, l.copy(at = cursor.Node.transformAfterInserted(wc, wcs.size, lc)))
          case l@operation.Node.Insert(lc, lcs) =>
            if (wc == lc) {
              Rebased(Set(conflict.Node.Asymmetry()), some(w, l.copy(at = cursor.Node.transformAfterInserted(wc, wcs.size, wc))))
            } else {
              free(w.copy(at = cursor.Node.transformAfterInserted(lc, lcs.size, wc)), l.copy(at = cursor.Node.transformAfterInserted(wc, wcs.size, lc)))
            }
          case d@operation.Node.Delete(_) =>
            insertDelete(w, d, conflict.Node.LoserDeletesWinner())
          case m@operation.Node.Move(_, _) =>
            insertMove(w, m)
        }
      case d@operation.Node.Delete(wr) =>
        loser match {
          case l@operation.Node.Content(lc, _) =>
            wr.transformAfterDeleted(lc) match {
              case Some(p) => free(winner, l.copy(at = p))
              case None => Rebased(Set(conflict.Node.WinnerDeletesLoser()), (Seq(winner), Seq.empty))
            }
          case l@operation.Node.Replace(lc, _) =>
            wr.transformAfterDeleted(lc) match {
              case Some(p) => free(winner, l.copy(at = p))
              case None => Rebased(Set(conflict.Node.WinnerDeletesLoser()), (Seq(winner), Seq.empty))
            }
          case i@operation.Node.Insert(_, _) =>
            reverse(insertDelete(i, d, conflict.Node.WinnerDeletesLoser()))
          case operation.Node.Delete(lr) =>
            val wp = lr.transformDeletingRangeAfterDeleted(wr).map(operation.Node.Delete).toSeq
            val lp = wr.transformDeletingRangeAfterDeleted(lr).map(operation.Node.Delete).toSeq
            Rebased(Set.empty, (wp, lp))
          case m@operation.Node.Move(_, _) =>
            deleteMove(d, m, conflict.Node.WinnerDeletesLoser())
        }
      case w@operation.Node.Move(wr, wa) =>
        loser match {
          case l@operation.Node.Content(lc, lo) =>
            free(w, l.copy(at = wr.transformNodeAfterMoved(wa, lc)))
          case l@operation.Node.Replace(lc, lo) =>
            free(w, l.copy(at = wr.transformNodeAfterMoved(wa, lc)))
          case i@operation.Node.Insert(lc, lcs) =>
            reverse(insertMove(i, w))
          case d@operation.Node.Delete(_) =>
            reverse(deleteMove(d, w, conflict.Node.LoserDeletesWinner()))
          case l@operation.Node.Move(lr, la) =>

            def freeReverseLFirst(w: operation.Node.Move, l: operation.Node.Move): RebaseResult = {
              (w, l) match {
                case (operation.Node.Move(wr, wa), operation.Node.Move(lr, la)) =>
                  if (wr.size == 0 || lr.size == 0) {
                    free(w, l)
                  } else {
                    val s2 = ignoreNullMoves(
                      range.Node(wr.transformNodeAfterMoved(wa, lr.start), moveBy(wr.transformNodeAfterMoved(wa, moveBy(lr.until, -1)), 1)),
                      wr.transformInsertionPointAfterMoved(wa, la))
                    free(
                      Seq(reverseMove(l), w) ++ s2,
                      s2
                    )
                  }
              }
            }
            def free0(): RebaseResult = {
              if (wr.size == 0 || lr.size == 0) {
                free(w, l)
              } else {
                free(
                  ignoreNullMoves(
                    range.Node(lr.transformNodeAfterMoved(la, wr.start), moveBy(lr.transformNodeAfterMoved(la, moveBy(wr.until, -1)), 1)),
                    lr.transformInsertionPointAfterMoved(la, wa)),
                  ignoreNullMoves(
                    range.Node(wr.transformNodeAfterMoved(wa, lr.start), moveBy(wr.transformNodeAfterMoved(wa, moveBy(lr.until, -1)), 1)),
                    wr.transformInsertionPointAfterMoved(wa, la))
                )
              }
            }
            def oneContains(w: operation.Node.Move, l: operation.Node.Move): RebaseResult = {
              (w, l) match {
                case (operation.Node.Move(wr, wa), operation.Node.Move(lr, la)) =>
                  val wstart = if (wr.start.last == 0) {
                    lr.transformNodeAfterMoved(la, wr.start.dropRight(1)) :+ 0
                  } else {
                    moveBy(lr.transformNodeAfterMoved(la, moveBy(wr.start, -1)), 1)
                  }
                  val wuntil = lr.transformNodeAfterMoved(la, wr.until)
                  free(
                    ignoreNullMoves(
                      range.Node(wstart, wuntil),
                      lr.transformInsertionPointAfterMoved(la, wa)),
                    ignoreNullMoves(
                      range.Node(wr.transformNodeAfterMoved(wa, lr.start), lr.size),
                      wr.transformTouchPointAfterMoved(wa, la))
                  )
              }
            }
            if (wa == la) { // no range contains any insertion point
              if (wr.parent == lr.parent && wr.childs.overlap(lr.childs)) {
                // if same level, and overlap, merge and move
                val r = wr.childs.merge(lr.childs)
                val ll = if (r == wr.childs) Seq.empty else Seq(reverseMove(w), l)
                val ww = if (r == lr.childs) Seq.empty else Seq(reverseMove(l), w)
                free(ww, ll)
              } else {
                // winner moves first
                free(
                  ignoreNullMoves(
                    range.Node(lr.transformNodeAfterMoved(la, wr.start), wr.size),
                    moveBy(lr.transformNodeAfterMoved(la, wa), -lr.childs.size)),
                  ignoreNullMoves(
                    range.Node(wr.transformNodeAfterMoved(wa, lr.start), lr.size),
                    wr.transformNodeAfterMoved(wa, la))
                )
              }
            } else {
              // considered one move inside another
              if (wr.contains(lr) && wr != lr) {
                if (wr.contains(la) || wr.touchsInsertionPoint(la)) {
                  oneContains(w, l)
                } else {
                  freeReverseLFirst(w, l)
                }
              } else if (lr.contains(wr) && wr != lr) {
                if (lr.contains(wa) || lr.touchsInsertionPoint(wa)) {
                  reverse(oneContains(l, w))
                } else {
                  reverse(freeReverseLFirst(l, w))
                }
              } else if ((lr.containsInsertionPoint(wa) && wr.containsInsertionPoint(la)) ||  // ill case, only winner performed
                lr.overlap(wr)) { // don't want to deal with this anymore
                println(s"overlap case $w $l")
                Rebased(Set(conflict.Node.Asymmetry()), (Seq(reverseMove(l), w), Seq.empty[operation.Node]))
              } else {
                free0() // try our luck
              }
            }
        }
    }
  }
}
