package shared.data

import boopickle.CompositePickler
import boopickle.Default._
import shared.data
import data.{Node => N}
import N.{Ref, Content => C}

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

sealed trait Change {
  def map(ref: N.Ref): N.Ref
  def map(ref: N.PointRef): N.PointRef
  def map(ref: N.SegmentRef): N.SegmentRef
  def rebase(o: Change): Change
}
object Change {
  case object Id extends Change {
    override def map(ref: N.Ref): N.Ref = ref
    override def map(ref: N.PointRef): N.PointRef = ref
    override def map(ref: N.SegmentRef): N.SegmentRef = ref
    override def rebase(o: Change): Change = o
  }

  sealed trait Node extends Change

  object Node {
    case class Delete(node: N.Ref) extends Node() {
      override def map(ref: N.Ref): N.Ref = ???
      override def map(ref: N.PointRef): N.PointRef = ???
      override def map(ref: N.SegmentRef): N.SegmentRef = ???
      override def rebase(o: Change): Change = ???
    }
    case class NewWithParent(parent: N.Ref, id: String) extends Node() {
      override def map(ref: N.Ref): N.Ref = ???
      override def map(ref: N.PointRef): N.PointRef = ???
      override def map(ref: N.SegmentRef): N.SegmentRef = ???
      override def rebase(o: Change): Change = ???
    }
    case class NewWithPrevious(previous: N.Ref, id: String) extends Node() {
      assert(previous.nonEmpty)
      override def map(ref: N.Ref): N.Ref = ???
      override def map(ref: N.PointRef): N.PointRef = ???
      override def map(ref: N.SegmentRef): N.SegmentRef = ???
      override def rebase(o: Change): Change = ???
    }
    case class Move(node: N.Ref, previous: N.Ref) extends Node() {
      override def map(ref: N.Ref): N.Ref = ???
      override def map(ref: N.PointRef): N.PointRef = ???
      override def map(ref: N.SegmentRef): N.SegmentRef = ???
      override def rebase(o: Change): Change = ???
    }
  }

  sealed trait Content extends Change
  object Content {
    case class Insert(point: N.PointRef, content: C) extends Content() {
      override def map(ref: N.Ref): N.Ref = ???
      override def map(ref: N.PointRef): N.PointRef = ???
      override def map(ref: N.SegmentRef): N.SegmentRef = ???
      override def rebase(o: Change): Change = ???
    }
    case class Delete(segment: N.SegmentRef) extends Content() {
      override def map(ref: N.Ref): N.Ref = ???
      override def map(ref: N.PointRef): N.PointRef = ???
      override def map(ref: N.SegmentRef): N.SegmentRef = ???
      override def rebase(o: Change): Change = ???
    }
    case class Move(segment: N.SegmentRef, to: N.PointRef) extends Content() {
      override def map(ref: N.Ref): N.Ref = ???
      override def map(ref: N.PointRef): N.PointRef = ???
      override def map(ref: N.SegmentRef): N.SegmentRef = ???
      override def rebase(o: Change): Change = ???
    }
  }

  def apply(root: data.Node, change: Change): data.Node = {
    change match {
      case n: Node.NewWithParent =>
        root.map(n.parent) { item =>
          item.copy(childs = N.empty(n.id) +: item.childs)
        }
      case n: Node.NewWithPrevious =>
        root.map(n.previous.dropRight(1)) { item =>
          val index = n.previous.last + 1
          item.copy(childs = item.childs.take(index) ++ Seq(N.empty(n.id)) ++ item.childs.drop(index))
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
    base.foldLeft(c) { (c, b) => b.rebase(c) }
  }
}

