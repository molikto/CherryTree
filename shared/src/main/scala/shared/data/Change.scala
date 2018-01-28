package shared.data

import boopickle.CompositePickler
import boopickle.Default._
import shared.data


trait ChangeImplicits {


  implicit val changeNodePickler: CompositePickler[Change.Node] =
    compositePickler[Change.Node]
      .addConcreteType[Change.Node.Delete]
      .addConcreteType[Change.Node.NewWithParent]
      .addConcreteType[Change.Node.NewWithPrevious]
      .addConcreteType[Change.Node.Move]

  implicit val changeContentPickler: CompositePickler[Change.Content] =
    compositePickler[Change.Content]
      .addConcreteType[Change.Content.Insert]
      .addConcreteType[Change.Content.Delete]
      .addConcreteType[Change.Content.Move]

  implicit val changePickler: CompositePickler[Change] =
    compositePickler[Change]
      .join[Change.Node](changeNodePickler)
      .join[Change.Content](changeContentPickler)
}

sealed trait Change
object Change {

  type N = data.Node.Ref
  type C = data.Node.Content

  sealed trait Node extends Change

  object Node {
    case class Delete(node: N) extends Node()
    case class NewWithParent(parent: N, id: String) extends Node()
    case class NewWithPrevious(previous: N, id: String) extends Node() {
      assert(previous.nonEmpty)
    }
    case class Move(node: N, previous: N) extends Node()
  }

  sealed trait Content extends Change
  object Content {
    case class Insert(point: data.Node.PointRef, content: C) extends Content()
    case class Delete(segment: data.Node.SegmentRef) extends Content()
    case class Move(segment: data.Node.SegmentRef, to: data.Node.PointRef) extends Content()
  }

  def apply(root: data.Node, change: Change): data.Node = {
    change match {
      case n: Node.NewWithParent =>
        root.map(n.parent) { item =>
          item.copy(childs = data.Node.empty(n.id) +: item.childs)
        }
      case n: Node.NewWithPrevious =>
        root.map(n.previous.dropRight(1)) { item =>
          val index = n.previous.last + 1
          item.copy(childs = item.childs.take(index) ++ Seq(data.Node.empty(n.id)) ++ item.childs.drop(index))
        }
      case d: Node.Delete =>
        root.map(d.node.dropRight(1)) { item =>
          val index = d.node.last
          item.copy(childs = item.childs.take(index) ++ item.childs.drop(index + 1))
        }
      case _ =>
        ???
    }
  }

  def rebase(base: Seq[Change], c: Change): Change = {
    ???
  }
}

