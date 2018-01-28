package shared.data

import boopickle.DefaultBasic.PicklerGenerator
import boopickle._
import shared.data

sealed trait Change
object Change {
  implicit val pickler: Pickler[Change] = PicklerGenerator.generatePickler[Change]

  type N = data.Node.Ref
  type C = data.Node.Content

  sealed trait Node extends Change
  object Node {
    case class Delete(node: N) extends Node()
    case class NewWithParent(parent: N) extends Node()
    case class NewWithPrevious(previous: N) extends Node()
    case class Move(node: N, previous: N) extends Node()
  }

  sealed trait Content extends Change
  object Content {
    case class Insert(point: data.Node.PointRef, content: C) extends Content()
    case class Delete(segment: data.Node.SegmentRef) extends Content()
    case class Move(segment: data.Node.SegmentRef, to: data.Node.PointRef) extends Content()
  }

  def apply(document: Document, changes: List[Change]): Document = {
    ???
  }
}

