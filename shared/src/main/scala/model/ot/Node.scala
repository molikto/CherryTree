package model.ot


import boopickle.Pickler
import model._

import scala.util.Random

object Node {
  class Conflict {

  }
  object Conflict {

  }
}
class Node extends Ot[data.Node, operation.Node, Node.Conflict] {

  type RebaseResult = Rebased[Unicode.Conflict, (Option[operation.Unicode], Option[operation.Unicode])]

  override def rebase(winner: operation.Node, loser: operation.Node): Rebased[Node.Conflict, (Option[operation.Node], Option[operation.Node])] =

  override def generateRandomChange(MODEL: data.Node, random: Random): operation.Node = ???

  override def generateRandomModel(random: Random): data.Node = ???

  override val dataPickler: Pickler[data.Node] = _
  override val operationPickler: Pickler[operation.Node] = _
}
