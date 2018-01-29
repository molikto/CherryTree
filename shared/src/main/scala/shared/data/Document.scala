package shared.data

import shared.data.Node.{Content, Ref}

import scala.util.{Random, Try}


object Node extends IdGenerator {

  case class Ref(v: Seq[Int]) {
    def disjoint(to: Ref): Boolean = !to.v.startsWith(v) && !v.startsWith(to.v)
    def intersect(to: Ref): Boolean = !disjoint(to)
    def parent: Ref = Ref(v.dropRight(1))
    def withChild(a: Int): Ref = Ref(v :+ a)
    def previous: Ref = parent.withChild(v.last - 1)
    def next: Ref = parent.withChild(v.last + 1)
    private[Node] def head: Int = v.head
    private[Node] def tail = Ref(v.tail)
    private[Node] def last: Int = v.last
  }

  object Ref {
    def root = Ref(Seq.empty)
  }
  type Content = String
  object Content {
    def insert(content: Content, contentPoint: PointRef, c: Content): Content = {
      content.substring(0, contentPoint) ++ c ++ content.substring(contentPoint)
    }

    def delete(content: Content, contentSegment: SegmentRef): (Content, Content) = {
      val res = content.substring(0, contentSegment.from) ++ content.substring(contentSegment.to + 1)
      val deleted = content.substring(contentSegment.from, contentSegment.to + 1)
      (res, deleted)
    }

    type PointRef = Int

    /**
      * @param from
      * @param to inclusive
      */
    case class SegmentRef(from: PointRef, to: PointRef) {
      assert(to >= from)
    }
    def empty: Content = ""
  }

  case class PointRef(child: Node.Ref, contentPoint: Content.PointRef) {
    def to(length: Int): SegmentRef = SegmentRef(child, Content.SegmentRef(contentPoint, contentPoint + length - 1))

  }

  case class SegmentRef(child: Node.Ref, contentSegment: Content.SegmentRef) {
    def from: PointRef = PointRef(child, contentSegment.from)
    def to: PointRef = PointRef(child, contentSegment.to)
  }

  def empty(id: String) = Node(id, Content.empty, Seq.empty)
}

case class Node(id: String, content: Node.Content, childs: Seq[Node]) extends  (Node.Ref => Node) {

  // This might fail

  override def apply(child: Node.Ref): Node = {
    if (child == Node.Ref.root) {
      this
    } else {
      childs(child.head)(child.tail)
    }
  }

  /**
    * @return pair of current node with child removed, and the removed node content
    */
  def delete(child: Node.Ref): (Node, Node) = {
    var removed: Node = null
    val res = map(child.parent) { item =>
      val index = child.last
      removed = item.childs(index)
      item.copy(childs = item.childs.take(index) ++ item.childs.drop(index + 1))
    }
    (res, removed)
  }

  def delete(child: Node.SegmentRef): (Node, Node.Content) = {
    var removed: Node.Content = null
    val res = map(child.child) { item =>
      val pair = Node.Content.delete(item.content, child.contentSegment)
      removed = pair._2
      item.copy(content = pair._1)
    }
    (res, removed)
  }


  def insert(point: Node.PointRef, content: Content): Node = {
    map(point.child) { item =>
      val cc = Node.Content.insert(item.content, point.contentPoint, content)
      item.copy(content = cc)
    }
  }

  import Node._
  /**
    * @return new node
    */
  def insert(p: Node.Ref, node: Node): Node = {
    map(p.parent) { item =>
      val index = p.last
      item.copy(childs = item.childs.take(index) ++ Seq(node) ++ item.childs.drop(index))
    }
  }

  def map(child: Node.Ref)(p: Node => Node): Node = {

    if (child == Node.Ref.root) {
      p(this)
    } else {
      val index = child.head
      copy(childs = childs.take(index) ++ Seq(childs(index).map(child.tail)(p)) ++ childs.drop(index + 1))
    }
  }
}


sealed class Mode {
}
object Mode {
  case class Normal(segment: Node.SegmentRef) extends Mode
  case class Insert(point: Node.PointRef) extends Mode
  case class Selection(segment: Node.SegmentRef) extends Mode
  case class SelectionTree(node: Node.Ref) extends Mode
}

case class Document(version: Int, root: Node)

object Document {
  def empty = Document(0, Node.empty(Node.newId()))
}

