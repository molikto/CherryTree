package model.ot

import boopickle.{PickleState, Pickler, UnpickleState}
import model._
import com.softwaremill.quicklens._
import model.range.IntRange

import scala.util.Random
import util._
import operation.Unicode._

object Unicode {

  class Conflict

  object Conflict {

    case class Asymmetry() extends Conflict

    case class WinnerDeletesLoser() extends Conflict

    case class LoserDeletesWinner() extends Conflict

    case class WinnerMovesLoser() extends Conflict
  }

}

class Unicode extends Ot[data.Unicode, operation.Unicode, Unicode.Conflict] {

  type RebaseResult = Rebased[Unicode.Conflict, (Option[operation.Unicode], Option[operation.Unicode])]

  override def rebase(winner: operation.Unicode, loser: operation.Unicode): RebaseResult = {
    def addDelete(add: Insert, delete: Delete, addIsWinner: Boolean): RebaseResult = {
      val wat = add.at
      val wc = add.unicode
      val lfrom = delete.r.start
      val lto = delete.r.endInclusive
      if (lfrom < wat && lto >= wat) {
        Rebased(Set(if (addIsWinner) Unicode.Conflict.LoserDeletesWinner() else Unicode.Conflict.WinnerDeletesLoser()), (
          None,
          Some(Delete(lfrom, lto + wc.size))
        ))
      } else {
        val (wat0, ld) = if (wat <= lfrom) (wat, wc.size) else (wat - delete.r.size, 0)
        Rebased(Set.empty, (
          Some(Insert(wat0, wc)),
          Some(Delete(lfrom + ld, lto + ld))
        ))
      }
    }

    def reverse(res: RebaseResult) = Rebased(res.conflicts, (res.t._2, res.t._1))

    (winner, loser) match {
      case (w@Insert(wat, wc), l@Insert(lat, lc)) =>
        if (wat == lat) {
          val at = wat
          Rebased(Set(Unicode.Conflict.Asymmetry()), (
            Some(Insert(at + lc.size, wc)),
            Some(loser)
          ))
        } else if (wat > lat) {
          Rebased(Set.empty, (Some(w.modify(_.at).using(_ + lc.size)), Some(l)))
        } else {
          Rebased(Set.empty, (Some(w), Some(l.modify(_.at).using(_ + wc.size))))
        }
      case (a@Insert(_, _), l@Delete(_)) =>
        addDelete(a, l, addIsWinner = true)
      case (d@Delete(_), a@Insert(_, _)) =>
        reverse(addDelete(a, d, addIsWinner = false))
      case (Delete(ws), Delete(ls)) =>
        val wp = ls.transformDeletingRangeAfterDeleted(ws).map(a => Delete(a.start, a.endInclusive))
        val lp = ws.transformDeletingRangeAfterDeleted(ls).map(a => Delete(a.start, a.endInclusive))
        Rebased(Set.empty, (wp, lp))
      case (Delete(_), Move(_, _)) =>
        ???
      case (Insert(_, _), Move(_, _)) =>
        ???
      case (Move(_, _), Delete(_)) =>
        ???
      case (Move(_, _), Insert(_, _)) =>
        ???
      case (Move(wr, wa), Move(lr, la)) =>
        ???
    }
  }


  override def generateRandomChange(d: data.Unicode, random: Random): operation.Unicode = {

    import model.{data, operation}

    if (random.nextBoolean() || d.isEmpty) {
      Insert(random.nextInt(d.size + 1), data.Unicode(random.nextLong().toString))
    } else {
      val (end, start) = maxMin(random.nextInt(d.size), random.nextInt(d.size))
      Delete(start, end)
    }
  }

  override def generateRandomData(random: Random): data.Unicode = data.Unicode(random.nextLong().toString)

  override val dataPickler: Pickler[data.Unicode] = data.Unicode.pickler

  override val operationPickler: Pickler[operation.Unicode] = operation.Unicode.pickler
}

