package shared.data

import boopickle.CompositePickler
import boopickle.Default._
import shared.data
import data.{Node => N}
import N.{Ref, Content => C}

import Node._

trait ChangeImplicits {

  implicit val changeNodePickler: CompositePickler[Change.Node] =
    compositePickler[Change.Node]
      .addConcreteType[Change.Node.Delete]
      .addConcreteType[Change.Node.Insert]

  implicit val changeContentPickler: CompositePickler[Change.Content] =
    compositePickler[Change.Content]
      .addConcreteType[Change.Content.Insert]
      .addConcreteType[Change.Content.Delete]

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
    case class Delete(position: N.Ref) extends Node() {
      override def map(ref: N.Ref): N.Ref = ???
      override def map(ref: N.PointRef): N.PointRef = ???
      override def map(ref: N.SegmentRef): N.SegmentRef = ???
      override def rebase(o: Change): Change = {
        o match {
          case d: Node.Delete =>
            val diff =
            if (d.position == position) {
              Id
            } else
          case i: Node.Insert =>
          case d: Content.Delete =>
          case i: Content.Insert =>
        }
      }
    }
    case class Insert(position: N.Ref, node: N) extends Node() {
      override def map(ref: N.Ref): N.Ref = ???
      override def map(ref: N.PointRef): N.PointRef = ???
      override def map(ref: N.SegmentRef): N.SegmentRef = ???
      override def rebase(o: Change): Change = ???
    }
  }

  sealed trait Content extends Change
  object Content {
    case class Insert(point: N.PointRef, content: C) extends Content() {
      assert(content.nonEmpty)
      override def map(ref: N.Ref): N.Ref = ref
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
  }

  /**
    * @return return a pair of new root, and a reverse change
    */
  def apply(root: data.Node, change: Change): (data.Node, Change) = {
    change match {
      case i: Node.Insert =>
        (root.insert(i.position, i.node), Change.Node.Delete(i.position))
      case d: Node.Delete =>
        val (node, content) = root.delete(d.position)
        (node, Change.Node.Insert(d.position, content))
      case c: Content.Delete =>
        val (node, content) = root.delete(c.segment)
        (node, Change.Content.Insert(c.segment.from, content))
      case c: Content.Insert =>
        (root.insert(c.point, c.content), Change.Content.Delete(c.point.to(c.content.length)))
      case Id =>
        (root, Id)
    }
  }

  def rebase(base: Seq[Change], c: Change): Change = {
    base.foldLeft(c) { (c, b) => b.rebase(c) }
  }
}

