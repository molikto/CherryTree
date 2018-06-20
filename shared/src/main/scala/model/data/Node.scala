package model.data

import model._

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

  def apply(range: range.Node): Seq[Node] = apply(range.parent).childs.slice(range.start, range.until)

  def delete(d: range.Node): Node = map(d.parent, a => a.copy(childs = a.childs.patch(d.start, Seq.empty, d.size)))

  private def insert(i: Int, childs: Seq[Node]): Node =
    copy(childs = childs.patch(i, childs, 0))

  def insert(cursor: cursor.Node, childs: Seq[Node]): data.Node =
    map(cursor.dropRight(1), a => a.insert(cursor.last, childs))

  def move(r: range.Node, at: cursor.Node): data.Node = {
    val a = apply(r)
    delete(r).insert(r.transformInsertionPointAfterDeleted(at), a)
  }
}
