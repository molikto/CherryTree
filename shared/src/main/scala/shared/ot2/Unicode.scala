package shared.ot2

import boopickle.{PickleState, Pickler, UnpickleState}
import shared.model
import shared.operation
import com.softwaremill.quicklens._

import scala.util.Random
import shared.util._

object Unicode {

  class Conflict

  object Conflict {

    case class Asymmetry() extends Conflict

    case class WinnerDeletesLoser() extends Conflict

    case class LoserDeletesWinner() extends Conflict

  }

}

class Unicode extends Ot[model.Unicode, operation.Unicode, Unicode.Conflict] {

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
        val ws = Segment(wfrom, wto)
        val ls = Segment(lfrom, lto)
        val wp = transformDeletingSegmentAfterDeleted(ls, ws).map(a => operation.Unicode.Delete(a.from, a.to))
        val lp = transformDeletingSegmentAfterDeleted(ws, ls).map(a => operation.Unicode.Delete(a.from, a.to))
        Rebased(Set.empty, (wp, lp))
    }
  }


  override def generateRandomChange(data: model.Unicode, random: Random): operation.Unicode = {

    import shared.{model, operation}

    if (random.nextBoolean() || data.isEmpty) {
      operation.Unicode.Insert(random.nextInt(data.size + 1), model.Unicode(random.nextLong().toString))
    } else {
      val (end, start) = maxMin(random.nextInt(data.size), random.nextInt(data.size))
      operation.Unicode.Delete(start, end)
    }
  }

  override def generateRandomModel(random: Random): model.Unicode = model.Unicode(random.nextLong().toString)

  override val dataPickler: Pickler[model.Unicode] = new Pickler[model.Unicode] {
    override def pickle(obj: model.Unicode)(implicit state: PickleState): Unit = state.enc.writeString(obj.toString)
    override def unpickle(implicit state: UnpickleState): model.Unicode = Unicode(state.dec.readString)
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
      }
    }
    override def unpickle(implicit state: UnpickleState): operation.Unicode = {
      import state.dec._
      readInt match {
        case 0 =>
          operation.Unicode.Insert(readInt, Unicode(readString))
        case 1 =>
          operation.Unicode.Delete(readInt, readInt)
      }
    }
  }
}

