package shared.data

import boopickle.CompositePickler
import boopickle.Default._
import shared.data
import shared.data.Node.{Content => C}
import shared.data.{Node => N}

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
  def mapOption(ref: N.Ref): Option[N.Ref]
  def mapOption(ref: N.PointRef): Option[N.PointRef]
  def mapOption(ref: N.SegmentRef): Option[N.SegmentRef]
  def rebaseOption(o: Change): Option[Change]
}
object Change {
  case object Id extends Change {
    override def mapOption(ref: N.Ref) = Some(ref)
    override def mapOption(ref: N.PointRef) = Some(ref)
    override def mapOption(ref: N.SegmentRef) = Some(ref)
    override def rebaseOption(o: Change) = Some(o)
  }

  sealed trait Node extends Change

  object Node {
    case class Delete(position: N.Ref) extends Node() {
      override def mapOption(ref: N.Ref) =
        N.Ref.transformAfterDeleted(position, ref)
      override def mapOption(ref: N.PointRef) =
        mapOption(ref.child).map(c => N.PointRef(c, ref.contentPoint))
      override def mapOption(ref: N.SegmentRef) =
        mapOption(ref.child).map(c => N.SegmentRef(c, ref.contentSegment))
      override def rebaseOption(o: Change) =
        o match {
          case d: Node.Delete =>
            // if we cannot find the deleted node, then we are fine!
            mapOption(d.position).map(c => Delete(c)).orElse(Some(Id))
          case i: Node.Insert =>
            if (i.position == position) {
              Some(i)
            } else {
              mapOption(i.position).map(c => Insert(c, i.node))
            }
          case d: Content.Delete =>
            // if we cannot find the deleted segment, we are fine!
            mapOption(d.segment.child)
              .map(c => d.copy(segment = d.segment.copy(child = c)))
              .orElse(Some(Id))
          case i: Content.Insert =>
            mapOption(i.point.child).map(c => i.copy(point = i.point.copy(child = c)))
        }
    }
    case class Insert(position: N.Ref, node: N) extends Node() {
      assert(position != N.Ref.root)
      override def mapOption(ref: N.Ref) =
        Some(N.Ref.transformAfterInserted(position, ref))
      override def mapOption(ref: N.PointRef) =
        mapOption(ref.child).map(c => N.PointRef(c, ref.contentPoint))
      override def mapOption(ref: N.SegmentRef) =
        mapOption(ref.child).map(c => N.SegmentRef(c, ref.contentSegment))
      override def rebaseOption(o: Change) =
        o match {
          case d: Node.Delete =>
            mapOption(d.position).map(c => Delete(c))
          case i: Node.Insert =>
            // weak conflict
            mapOption(i.position).map(c => Insert(c, i.node))
          case d: Content.Delete =>
            mapOption(d.segment.child).map(c => d.copy(segment = d.segment.copy(child = c)))
          case i: Content.Insert =>
            mapOption(i.point.child).map(c => i.copy(point = i.point.copy(child = c)))
        }
    }
  }

  sealed trait Content extends Change
  object Content {
    case class Insert(point: N.PointRef, content: C) extends Content() {
      assert(content.nonEmpty)
      override def mapOption(ref: N.Ref) = Some(ref)

      override def mapOption(ref: N.PointRef) = {
        val res =
          if (point.child == ref.child)
            ref.copy(contentPoint = C.transformAfterInserted(point.contentPoint, content.length, ref.contentPoint))
          else ref
        Some(res)
      }

      override def mapOption(ref: N.SegmentRef) = {
        val res =
          if (point.child == ref.child)
            ref.copy(contentSegment = C.SegmentRef(
              C.transformAfterInserted(point.contentPoint, content.length, ref.contentSegment.from),
              C.transformAfterInserted(point.contentPoint, content.length, ref.contentSegment.to)
            ))
          else ref
        Some(res)
      }
      override def rebaseOption(o: Change) =
        o match {
          case d: Node.Delete => Some(d)
          case i: Node.Insert => Some(i)
          case d: Content.Delete =>
            if (d.segment.child == point.child) {
              mapOption(d.segment).map(s => d.copy(segment = s))
            } else {
              Some(d)
            }
          case i: Content.Insert =>
            if (i.point.child == point.child) {
              mapOption(i.point).map(s => i.copy(point = s))
            } else {
              Some(i)
            }
        }
    }
    case class Delete(segment: N.SegmentRef) extends Content() {
      override def mapOption(ref: N.Ref) = Some(ref)

      override def mapOption(ref: N.PointRef) = {
        if (segment.child == ref.child)
          C.transformAfterDeleted(segment.contentSegment, ref.contentPoint).map(s => ref.copy(contentPoint = s))
        else Some(ref)
      }

      override def mapOption(ref: N.SegmentRef) = {
        val res =
          if (point.child == ref.child)
            ref.copy(contentSegment = C.SegmentRef(
              C.transformAfterInserted(point.contentPoint, content.length, ref.contentSegment.from),
              C.transformAfterInserted(point.contentPoint, content.length, ref.contentSegment.to)
            ))
          else ref
        Some(res)
      }
      override def rebaseOption(o: Change) =
        o match {
          case d: Node.Delete => Some(d)
          case i: Node.Insert => Some(i)
          case d: Content.Delete =>
            if (d.segment.child == point.child) {
              mapOption(d.segment).map(s => d.copy(segment = s))
            } else {
              Some(d)
            }
          case i: Content.Insert =>
            if (i.point.child == point.child) {
              mapOption(i.point).map(s => i.copy(point = s))
            } else {
              Some(i)
            }
        }
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

