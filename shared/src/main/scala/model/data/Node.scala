package model.data

import boopickle._
import model._
import model.range.IntRange

import scala.util.Random

// TODO simple type of node, so that it can be article, ordered list, unordered list, quote
case class Node(content: Content, childs: Seq[Node]) {

  def map(c: cursor.Node, transform: Node => Node): Node = {
    if (c.isEmpty) {
      transform(this)
    } else {
      copy(childs = childs.patch(c.head, Seq(childs(c.head).map(c.tail, transform)), 1))
    }
  }

  def apply(c: cursor.Node): Node = if (c.isEmpty) this else childs(c.head)(c.tail)

  def apply(r: range.Node): Seq[Node] = this(r.parent)(r.childs)

  def apply(r: IntRange): Seq[Node] = childs.slice(r.start, r.until)

  def delete(d: range.Node): Node = map(d.parent, _.delete(d.childs))

  private def delete(r: IntRange): Node =
    copy(childs = childs.patch(r.start, Seq.empty, r.size))

  private def insert(i: Int, childs: Seq[Node]): Node =
    copy(childs = childs.patch(i, childs, 0))

  def insert(c: cursor.Node, childs: Seq[Node]): data.Node =
    map(c.dropRight(1), a => a.insert(c.last, childs))

  def move(r: range.Node, at: cursor.Node): data.Node = {
    val a = this(r)
    delete(r).insert(r.transformInsertionPointAfterDeleted(at), a)
  }
}

object Node extends DataObject[Node] {
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

  override def random(random: Random): Node = ???
}
