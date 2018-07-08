package model

import model._
import model.operation.OperationObject
import model.ot.{Rebased, _}
import util._

import scala.util.Random


package object ot {
  
  trait Ot[DATA, OPERATION <: operation.Operation[DATA], CONFLICT] {

    type TRANSACTION = Seq[OPERATION]

    // LATER should here be a data: DATA??
    def rebase(winner: OPERATION, loser: OPERATION): Rebased[CONFLICT, (Seq[OPERATION], Seq[OPERATION])]

    def free(winner: OPERATION, loser: OPERATION): Rebased[CONFLICT, (Seq[OPERATION], Seq[OPERATION])] =
      Rebased(Set.empty, some(winner, loser))

    def free(winner: Seq[OPERATION], loser: Seq[OPERATION]): Rebased[CONFLICT, (Seq[OPERATION], Seq[OPERATION])] =
      Rebased(Set.empty, (winner, loser))

    def rebase(winner: Option[OPERATION], loser: OPERATION): Rebased[CONFLICT, (Seq[OPERATION], Seq[OPERATION])] = {
      winner match {
        case Some(a) => rebase(winner, loser)
        case None => Rebased(Set.empty, (Seq.empty, Seq(loser)))
      }
    }

    def rebase(winner: Option[OPERATION], loser: Option[OPERATION]): Rebased[CONFLICT, (Seq[OPERATION], Seq[OPERATION])] = {
      (winner, loser) match {
        case (None, None) => Rebased(Set.empty, (Seq.empty, Seq.empty))
        case (Some(a), _) => rebase(a, loser)
        case (None, Some(b)) => rebase(None, b)
      }
    }

    def rebase(winner: OPERATION, loser: Option[OPERATION]): Rebased[CONFLICT, (Seq[OPERATION], Seq[OPERATION])] = {
      loser match {
        case Some(l) => rebase(winner, l)
        case None => Rebased(Set.empty, (Seq(winner), Seq.empty))
      }
    }

    def rebase(winner: OPERATION, loser: TRANSACTION): Rebased[CONFLICT, (Seq[OPERATION], TRANSACTION)] = {
      loser.foldLeft(Rebased(Set.empty[CONFLICT], (Seq(winner): Seq[OPERATION], Seq.empty[OPERATION]))) { (pair, ll) =>
        pair match {
          case Rebased(t, (wi, lp)) =>
            val Rebased(t0, (wi0, lp0)) = wi match {
              case Nil => rebase(None, ll)
              case single +: Nil => rebase(single, ll)
              case wii => rebase(wii, ll)
            }
            Rebased(t ++ t0, (wi0, lp ++ lp0))
        }
      }
    }

    def rebase(winner: TRANSACTION, loser: OPERATION): Rebased[CONFLICT, (TRANSACTION, Seq[OPERATION])] = {
      winner.foldLeft(Rebased(Set.empty[CONFLICT], (Seq.empty[OPERATION], Seq(loser): Seq[OPERATION]))) { (pair, ww) =>
        pair match {
          case Rebased(t, (wp, li)) =>
            val Rebased(t0, (wp0, li0)) = li match {
              case Nil => rebase(ww, None)
              case single +: Nil => rebase(ww, single)
              case lii => rebase(ww, lii)
            }
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
  }

  case class Rebased[CONFLICT, T](conflicts: Set[CONFLICT], t: T) {
    def map[G](map: T => G) = Rebased(conflicts, map(t))
  }
}
