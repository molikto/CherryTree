package model.data

import model._
import model.range.IntRange

import scala.util.Random

// LATER simple type of node, so that it can be article, ordered list, unordered list, quote
case class Node(content: Content, childs: Seq[Node]) {
  def rich : Rich = content.asInstanceOf[Content.Rich].content


  def map(c: cursor.Node, transform: Node => Node): Node = {
    if (c.isEmpty) {
      transform(this)
    } else {
      copy(childs = childs.patch(c.head, Seq(childs(c.head).map(c.tail, transform)), 1))
    }
  }

  def get(a: cursor.Node): Option[Node] =
    if (a.isEmpty) Some(this) else if (a.head >= childs.size) None else childs(a.head).get(a.tail)

  def apply(c: cursor.Node): Node = if (c.isEmpty) this else childs(c.head)(c.tail)

  def apply(r: range.Node): Seq[Node] = this(r.parent)(r.childs)

  def apply(r: IntRange): Seq[Node] = childs.slice(r.start, r.until)

  def delete(d: range.Node): Node = map(d.parent, _.delete(d.childs))

  private def delete(r: IntRange): Node =
    copy(childs = childs.patch(r.start, Seq.empty, r.size))

  private def insert(i: Int, cs: Seq[Node]): Node = {
    if (i < 0 || i > childs.size) throw new IllegalArgumentException("Insertion is out of bound")
    copy(childs = childs.patch(i, cs, 0))
  }

  def insert(c: cursor.Node, cs: Seq[Node]): data.Node =
    map(c.dropRight(1), a => a.insert(c.last, cs))

  def move(r: range.Node, at: cursor.Node): data.Node = {
    val a = this(r)
    delete(r).insert(r.transformAfterDeleted(at).get, a)
  }
}

object Node extends DataObject[Node] {
  def defaultNormalMode(root: Node, node: cursor.Node): mode.Node.Content = {
    model.mode.Node.Content(node, root(node).content.defaultNormalMode())
  }


  val empty = Node(data.Content.Rich(data.Rich.empty), Seq.empty)
  val pickler: Pickler[Node] = new Pickler[Node] {
    override def pickle(obj: Node)(implicit state: PickleState): Unit = {
      import state.enc._
      Content.pickler.pickle(obj.content)
      writeInt(obj.childs.size)
      for (c <- obj.childs) Node.pickler.pickle(c)
    }

    override def unpickle(implicit state: UnpickleState): Node = {
      import state.dec._
      Node(Content.pickler.unpickle, (0 until readInt).map(_ => Node.pickler.unpickle))
    }
  }

  override def random(r: Random): Node = randomWithDepth(r, 0)

  private def randomWithDepth(r: Random, depth: Int): Node = {
    val childsAtDepth = depth match {
      case 0 => 5
      case 1 => 4
      case 2 => 4
      case 3 => 2
      case _ => 1
    }
    data.Node(data.Content.random(r), (0 until r.nextInt(childsAtDepth)).map(_ => randomWithDepth(r, depth + 1)))
  }
}
