package model.data

import model._
import range.{IntRange, Node}

case class Node(content: Content, childs: Seq[Node]) {

  def map(cursor: cursor.Node, transform: Node => Node): Node = {
    if (cursor.isEmpty) {
      transform(this)
    } else {
      copy(childs = childs.patch(cursor.head, Seq(childs(cursor.head).map(cursor.tail, transform)), 1))
    }
  }

  def apply(cursor: cursor.Node): Node = map(cursor, a => a)

  def apply(range: range.Node): Seq[Node] = apply(range.parent).childs.slice(range.start, range.until)

  private def insert(i: Int, childs: Seq[Node]): Node =
    copy(childs = childs.patch(i, childs, 0))

  def delete(d: range.Node): Node = map(d.parent, a => a.copy(childs = a.childs.patch(d.start, Seq.empty, d.size)))

  def insert(cursor: cursor.Node, childs: Seq[Node]): data.Node =
    map(cursor.dropRight(1), a => a.insert(cursor.last, childs))

  def move(r: range.Node, at: cursor.Node): data.Node = {
    val a = apply(r)
    delete(r).insert(r.transformInsertionPointAfterDeleted(at), a)
  }
}
