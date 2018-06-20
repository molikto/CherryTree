package model.ot

import boopickle.{PickleState, Pickler, UnpickleState}
import model._
import com.softwaremill.quicklens._

import scala.util.Random
import util._

object Unicode {

  class Conflict

  object Conflict {

    case class Asymmetry() extends Conflict

    case class WinnerDeletesLoser() extends Conflict

    case class LoserDeletesWinner() extends Conflict

  }

}

class Unicode extends Ot[data.Unicode, operation.Unicode, Unicode.Conflict] {

  type RebaseResult = Rebased[Unicode.Conflict, (Option[operation.Unicode], Option[operation.Unicode])]

  override def rebase(winner: operation.Unicode, loser: operation.Unicode): RebaseResult = {
    def addDelete(add: operation.Unicode.Insert, delete: operation.Unicode.Delete, addIsWinner: Boolean): RebaseResult = {
      val wat = add.at
      val wc = add.unicode
      val lfrom = delete.start
      val lto = delete.endInclusive
      if (lfrom < wat && lto >= wat) {
        Rebased(Set(if (addIsWinner) Unicode.Conflict.LoserDeletesWinner() else Unicode.Conflict.WinnerDeletesLoser()), (
          None,
          Some(operation.Unicode.Delete(lfrom, lto + wc.size))
        ))
      } else {
        val (wat0, ld) = if (wat <= lfrom) (wat, wc.size) else (wat - delete.size, 0)
        Rebased(Set.empty, (
          Some(operation.Unicode.Insert(wat0, wc)),
          Some(operation.Unicode.Delete(lfrom + ld, lto + ld))
        ))
      }
    }

    def reverse(res: RebaseResult) = Rebased(res.conflicts, (res.t._2, res.t._1))

    // TODO [warn] It would fail on the following inputs: (Delete(_, _), Move(_, _)), (Insert(_, _), Move(_, _)), (Move(_, _), Delete(_, _)), (Move(_, _), Insert(_, _)), (Move(_, _), Move(_, _))
    (winner, loser) match {
      case (w@operation.Unicode.Insert(wat, wc), l@operation.Unicode.Insert(lat, lc)) =>
        if (wat == lat) {
          val at = wat
          Rebased(Set(Unicode.Conflict.Asymmetry()), (
            Some(operation.Unicode.Insert(at + lc.size, wc)),
            Some(loser)
          ))
        } else if (wat > lat) {
          Rebased(Set.empty, (Some(w.modify(_.at).using(_ + lc.size)), Some(l)))
        } else {
          Rebased(Set.empty, (Some(w), Some(l.modify(_.at).using(_ + wc.size))))
        }
      case (a@operation.Unicode.Insert(_, _), l@operation.Unicode.Delete(_, _)) =>
        addDelete(a, l, addIsWinner = true)
      case (d@operation.Unicode.Delete(_, _), a@operation.Unicode.Insert(_, _)) =>
        reverse(addDelete(a, d, addIsWinner = false))
      case (operation.Unicode.Delete(wfrom, wto), operation.Unicode.Delete(lfrom, lto)) =>
        val ws = range.IntRange(wfrom, wto)
        val ls = range.IntRange(lfrom, lto)
        val wp = ls.transformDeletingRangeAfterDeleted(ws).map(a => operation.Unicode.Delete(a.start, a.endInclusive))
        val lp = ws.transformDeletingRangeAfterDeleted(ls).map(a => operation.Unicode.Delete(a.start, a.endInclusive))
        Rebased(Set.empty, (wp, lp))
    }
  }


  override def generateRandomChange(d: data.Unicode, random: Random): operation.Unicode = {

    import model.{data, operation}

    if (random.nextBoolean() || d.isEmpty) {
      operation.Unicode.Insert(random.nextInt(d.size + 1), data.Unicode(random.nextLong().toString))
    } else {
      val (end, start) = maxMin(random.nextInt(d.size), random.nextInt(d.size))
      operation.Unicode.Delete(start, end)
    }
  }

  override def generateRandomData(random: Random): data.Unicode = data.Unicode(random.nextLong().toString)

  override val dataPickler: Pickler[data.Unicode] = new Pickler[data.Unicode] {
    override def pickle(obj: data.Unicode)(implicit state: PickleState): Unit = state.enc.writeString(obj.toString)
    override def unpickle(implicit state: UnpickleState): data.Unicode = data.Unicode(state.dec.readString)
  }

  override val operationPickler: Pickler[operation.Unicode] = new Pickler[operation.Unicode] {
    override def pickle(obj: operation.Unicode)(implicit state: PickleState): Unit = {
      import state.enc._
      obj match {
        case operation.Unicode.Insert(at, childs) =>
          writeInt(0)
          writeInt(at)
          writeString(childs.toString)
        case operation.Unicode.Delete(from, to) =>
          writeInt(1)
          writeInt(from)
          writeInt(to)
        case operation.Unicode.Move(r, at) =>
          writeInt(2)
          range.IntRange.pickler.pickle(r)
          writeInt(at)
      }
    }
    override def unpickle(implicit state: UnpickleState): operation.Unicode = {
      import state.dec._
      readInt match {
        case 0 =>
          operation.Unicode.Insert(readInt, data.Unicode(readString))
        case 1 =>
          operation.Unicode.Delete(readInt, readInt)
        case 2 =>
          operation.Unicode.Move(range.IntRange.pickler.unpickle, readInt)
      }
    }
  }
}

