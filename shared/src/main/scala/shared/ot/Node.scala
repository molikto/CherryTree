package shared.ot


import boopickle.Pickler
import shared._

import scala.util.Random

object Node {
  class Conflict {

  }
  object Conflict {

  }
}
class Node extends Ot[model.Node, operation.Node, Node.Conflict] {

  type RebaseResult = Rebased[Unicode.Conflict, (Option[operation.Unicode], Option[operation.Unicode])]

  override def rebase(winner: operation.Node, loser: operation.Node): Rebased[Node.Conflict, (Option[operation.Node], Option[operation.Node])] =

  override def generateRandomChange(MODEL: model.Node, random: Random): operation.Node = ???

  override def generateRandomModel(random: Random): model.Node = ???

  override val dataPickler: Pickler[model.Node] = _
  override val operationPickler: Pickler[operation.Node] = _
}
