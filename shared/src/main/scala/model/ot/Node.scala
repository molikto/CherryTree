package model.ot


import boopickle.Pickler
import model._

import scala.util.Random

object Node extends Ot[data.Node, operation.Node, conflict.Node] {

  type RebaseResult = Rebased[conflict.Unicode, (Option[operation.Unicode], Option[operation.Unicode])]

  override def rebase(winner: operation.Node, loser: operation.Node): Rebased[conflict.Node, (Option[operation.Node], Option[operation.Node])] = {
    winner match {
      case operation.Node.Content(wc, wo) =>
        loser match {
          case operation.Node.Content(lc, lo) =>
            ???
          case operation.Node.Replace(lc, lo) =>
            ???
          case operation.Node.Insert(lc, lcs) =>
            ???
          case operation.Node.Delete(lr) =>
            ???
          case operation.Node.Move(lr, la) =>
            ???
        }
      case operation.Node.Replace(wc, wo) =>
        loser match {
          case operation.Node.Content(lc, lo) =>
            ???
          case operation.Node.Replace(lc, lo) =>
            ???
          case operation.Node.Insert(lc, lcs) =>
            ???
          case operation.Node.Delete(lr) =>
            ???
          case operation.Node.Move(lr, la) =>
            ???
        }
      case operation.Node.Insert(wc, wcs) =>
        loser match {
          case operation.Node.Content(lc, lo) =>
            ???
          case operation.Node.Replace(lc, lo) =>
            ???
          case operation.Node.Insert(lc, lcs) =>
            ???
          case operation.Node.Delete(lr) =>
            ???
          case operation.Node.Move(lr, la) =>
            ???
        }
      case operation.Node.Delete(wr) =>
        loser match {
          case operation.Node.Content(lc, lo) =>
            ???
          case operation.Node.Replace(lc, lo) =>
            ???
          case operation.Node.Insert(lc, lcs) =>
            ???
          case operation.Node.Delete(lr) =>
            ???
          case operation.Node.Move(lr, la) =>
            ???
        }
      case operation.Node.Move(wr, wa) =>
        loser match {
          case operation.Node.Content(lc, lo) =>
            ???
          case operation.Node.Replace(lc, lo) =>
            ???
          case operation.Node.Insert(lc, lcs) =>
            ???
          case operation.Node.Delete(lr) =>
            ???
          case operation.Node.Move(lr, la) =>
            ???
        }
    }
  }

  override def generateRandomChange(MODEL: data.Node, random: Random): operation.Node = ???

  override def generateRandomData(random: Random): data.Node = ???

  override val dataPickler: Pickler[data.Node] = data.Node.pickler
  override val operationPickler: Pickler[operation.Node] = operation.Node.pickler
}
