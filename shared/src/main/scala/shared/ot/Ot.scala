package shared.ot

import boopickle.Pickler
import shared.api

import scala.util.Random


trait OtOperation[T] {
  def information: Int
}

object Information {
  val Add = 1
  val Delete = 2
  val AddDelete = 3
}


case class Rebased[CONFLICT, T](conflicts: Set[CONFLICT], t: T) {
  def map[G](map: T => G) = Rebased(conflicts, map(t))
}

trait Ot[DATA, OPERATION <: OtOperation[DATA], CONFLICT] {


  type TRANSACTION = Seq[OPERATION]

  def apply(c: OPERATION, data: DATA): DATA

  // TODO should here be a data: DATA??
  def rebase(winner: OPERATION, loser: OPERATION): Rebased[CONFLICT, (Option[OPERATION], Option[OPERATION])]


  def generateRandomChange(data: DATA): OPERATION = generateRandomChange(data, new Random())
  def generateRandomChange(data: DATA, random: Random): OPERATION
  def generateRandomData(): DATA = generateRandomData(new Random())
  def generateRandomData(random: Random): DATA

  def generateRandomTransaction(size: Int, data: DATA): TRANSACTION = {
    var a = data
    var i = 0
    val r = new Random()
    var cs = Seq.empty[OPERATION]
    while (i < size) {
      val c = generateRandomChange(a, r)
      a = apply(c, a)
      cs = cs :+ c
      i += 1
    }
    cs
  }

  /****
    *
    */



  lazy val seqOt: Ot[Seq[DATA], SeqOperation[DATA, OPERATION], SeqConflict[DATA, CONFLICT]] =
    new SeqOt(this)

  //def empty: DATA

  def apply(c: Option[OPERATION], data: DATA): DATA = c match {
    case None => data
    case Some(a) => apply(a, data)
  }

  def apply(cs: Seq[OPERATION], data: DATA): DATA = {
    cs.foldLeft(data) { (data, c) => apply(c, data) }
  }

  def applyT(cs: Seq[TRANSACTION], data: DATA): DATA = {
    cs.foldLeft(data) { (data, c) => apply(c, data) }
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

  def rebase(winner: OPERATION, loser: TRANSACTION): Rebased[CONFLICT, (Option[OPERATION], Seq[OPERATION])] = {
    loser.foldLeft(Rebased(Set.empty[CONFLICT], (Some(winner): Option[OPERATION], Seq.empty[OPERATION]))) { (pair, ll) =>
      pair match {
        case Rebased(t, (wi, lp)) =>
          val Rebased(t0, (wi0, lp0)) = rebase(wi, ll)
          Rebased(t ++ t0, (wi0, lp ++ lp0))
      }
    }
  }

  def rebase(winner: Seq[OPERATION], loser: OPERATION): Rebased[CONFLICT, (Seq[OPERATION], Option[OPERATION])] = {
    winner.foldLeft(Rebased(Set.empty[CONFLICT], (Seq.empty[OPERATION], Some(loser) : Option[OPERATION]))) { (pair, ww) =>
      pair match {
        case Rebased(t, (wp, li)) =>
          val Rebased(t0, (wp0, li0)) = rebase(ww, li)
          Rebased(t ++ t0, (wp ++ wp0, li0))
      }
    }
  }

  def rebase(winner: Seq[OPERATION], loser: Seq[OPERATION]): Rebased[CONFLICT, (Seq[OPERATION], Seq[OPERATION])] = {
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
  def rebaseT(winner: Seq[OPERATION], loser: Seq[TRANSACTION]): Rebased[CONFLICT, (Seq[OPERATION], Seq[TRANSACTION])] = {
    loser.foldLeft(Rebased(Set.empty[CONFLICT], (winner, Seq.empty[TRANSACTION]))) { (pair, ll) =>
      pair match {
        case Rebased(t, (wi, lp)) =>
          val Rebased(t0, (wi0, lp0)) = rebase(wi, ll)
          val ret = if (lp0.isEmpty) lp else lp :+ lp0
          Rebased(t ++ t0, (wi0, ret))
      }
    }
  }

  val dataPickler: Pickler[DATA]
  val operationPickler: Pickler[OPERATION]
}


