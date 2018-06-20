package shared

import boopickle.Pickler
import shared.ot._


import scala.util.Random

import shared.util._

package object ot2 {
  
  trait Ot[MODEL, OPERATION <: operation.Operation[MODEL], CONFLICT] {
    type TRANSACTION = Seq[OPERATION]

    // LATER should here be a MODEL: MODEL??
    def rebase(winner: OPERATION, loser: OPERATION): Rebased[CONFLICT, (Option[OPERATION], Option[OPERATION])]


    def generateRandomChange(MODEL: MODEL): OPERATION = generateRandomChange(MODEL, new Random())

    def generateRandomChange(MODEL: MODEL, random: Random): OPERATION

    def generateRandomModel(): MODEL = generateRandomModel(new Random())

    def generateRandomModel(random: Random): MODEL

    def generateRandomTransaction(size: Int, MODEL: MODEL): TRANSACTION = {
      var a = MODEL
      var i = 0
      val r = new Random()
      var cs = Seq.empty[OPERATION]
      while (i < size) {
        val c = generateRandomChange(a, r)
        a = c.apply(a)
        cs = cs :+ c
        i += 1
      }
      cs
    }


    def apply(c: Option[OPERATION], model: MODEL): MODEL = c match {
      case None => model
      case Some(a) => a.apply(model)
    }

    def apply(cs: TRANSACTION, model: MODEL): MODEL = {
      cs.foldLeft(model) { (model, c) => c.apply(model) }
    }

    def applyT(cs: Seq[TRANSACTION], model: MODEL): MODEL = {
      cs.foldLeft(model) { (model, c) => apply(c, model) }
    }

    def rebase(winner: Option[OPERATION], loser: OPERATION): Rebased[CONFLICT, (Option[OPERATION], Option[OPERATION])] = {
      winner match {
        case Some(a) => rebase(winner, loser)
        case None => Rebased(Set.empty, (None, Some(loser)))
      }
    }

    def rebase(winner: Option[OPERATION], loser: Option[OPERATION]): Rebased[CONFLICT, (Option[OPERATION], Option[OPERATION])] = {
      (winner, loser) match {
        case (None, None) => Rebased(Set.empty, (None, None))
        case (Some(a), _) => rebase(a, loser)
        case (None, Some(b)) => rebase(None, b)
      }
    }

    def rebase(winner: OPERATION, loser: Option[OPERATION]): Rebased[CONFLICT, (Option[OPERATION], Option[OPERATION])] = {
      loser match {
        case Some(l) => rebase(winner, l)
        case None => Rebased(Set.empty, (Some(winner), None))
      }
    }

    def rebase(winner: OPERATION, loser: TRANSACTION): Rebased[CONFLICT, (Option[OPERATION], TRANSACTION)] = {
      loser.foldLeft(Rebased(Set.empty[CONFLICT], (Some(winner): Option[OPERATION], Seq.empty[OPERATION]))) { (pair, ll) =>
        pair match {
          case Rebased(t, (wi, lp)) =>
            val Rebased(t0, (wi0, lp0)) = rebase(wi, ll)
            Rebased(t ++ t0, (wi0, lp ++ lp0))
        }
      }
    }

    def rebase(winner: TRANSACTION, loser: OPERATION): Rebased[CONFLICT, (TRANSACTION, Option[OPERATION])] = {
      winner.foldLeft(Rebased(Set.empty[CONFLICT], (Seq.empty[OPERATION], Some(loser): Option[OPERATION]))) { (pair, ww) =>
        pair match {
          case Rebased(t, (wp, li)) =>
            val Rebased(t0, (wp0, li0)) = rebase(ww, li)
            Rebased(t ++ t0, (wp ++ wp0, li0))
        }
      }
    }

    def rebase(winner: TRANSACTION, loser: TRANSACTION): Rebased[CONFLICT, (TRANSACTION, TRANSACTION)] = {
      loser.foldLeft(Rebased(Set.empty[CONFLICT], (winner, Seq.empty[OPERATION]))) { (pair, ll) =>
        pair match {
          case Rebased(t, (wi, lp)) =>
            val Rebased(t0, (wi0, lp0)) = rebase(wi, ll)
            Rebased(t ++ t0, (wi0, lp ++ lp0))
        }
      }
    }

    /**
      * we take a winner seq op, because we know winner will always win, and get applied,
      *
      * for loser, we return a seq. not necessarily the same length
      */
    def rebaseT(winner: TRANSACTION, loser: Seq[TRANSACTION]): Rebased[CONFLICT, (TRANSACTION, Seq[TRANSACTION])] = {
      loser.foldLeft(Rebased(Set.empty[CONFLICT], (winner, Seq.empty[TRANSACTION]))) { (pair, ll) =>
        pair match {
          case Rebased(t, (wi, lp)) =>
            val Rebased(t0, (wi0, lp0)) = rebase(wi, ll)
            val ret = if (lp0.isEmpty) lp else lp :+ lp0
            Rebased(t ++ t0, (wi0, ret))
        }
      }
    }

    val dataPickler: Pickler[MODEL]
    val operationPickler: Pickler[OPERATION]
  }

  case class Rebased[CONFLICT, T](conflicts: Set[CONFLICT], t: T) {
    def map[G](map: T => G) = Rebased(conflicts, map(t))
  }



  case class Segment(from: Int, to: Int) {
    def contains(p: Int): Boolean = p >= from && p <= to
    def size: Int = to - from + 1
  }


  def transformAfterAdded(point: Int, size: Int, p: Int): Int = {
    if (p < point) {
      p
    } else {
      p + size
    }
  }

  def transformAfterDeleted(s: Segment, p: Int): Option[Int] = {
    if (p < s.from) {
      Some(p)
    } else if (s.contains(p)) {
      None
    } else {
      Some(p - s.size)
    }
  }

  /**
    * @return None if either side of `s` is deleted
    */
  def transformAfterDeleted(d: Segment, f: Segment): Option[Segment] = {
    val l = transformAfterDeleted(d, f.from)
    val r = transformAfterDeleted(d, f.to)
    (l, r) match {
      case (Some(ll), Some(rr)) => Some(Segment(ll, rr))
      case _ => None
    }
  }

  def transformDeletingSegmentAfterDeleted(d: Segment, f: Segment): Option[Segment] = {
    val l = transformAfterDeleted(d, f.from)
    val r = transformAfterDeleted(d, f.to)
    (l, r) match {
      case (Some(ll), Some(rr)) => Some(Segment(ll, rr))
      case (Some(ll), None) => Some(Segment(ll, d.from - 1))
      case (None, Some(rr)) => Some(Segment(d.from, rr))
      case (None, None) =>  None
    }
  }
}
