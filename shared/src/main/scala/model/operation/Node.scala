package model.operation

import model._
import Type.Type
import com.softwaremill.quicklens._
import model.range.IntRange

import scala.util.Random


abstract sealed class Node extends Operation[data.Node] {
  /**
    * the returned cursor, always assumes the the child list is not empty in case of a delete operation
    * this is harmless now, because it will point to a imaginary node, and this can be recalibrate afterwards
    */
  def transform(a: mode.Node): Option[mode.Node]
  def transform(a: Option[mode.Node]): Option[mode.Node] = a.flatMap(transform)
}

object Node extends OperationObject[data.Node, Node] {
  def transform(ns: Seq[Node], a: mode.Node): Option[mode.Node] = {
    ns.foldLeft(Some(a) : Option[mode.Node]) { (a, n) => n.transform(a) }
  }

  case class Content(at: cursor.Node, content: operation.Content) extends Node {
    override def ty: Type = content.ty
    override def apply(d: data.Node): data.Node = {
      d.map(at, a => a.copy(content = content(a.content)))
    }

    override def transform(a: mode.Node): Option[mode.Node] = a match {
      case c: mode.Node.Content if c.node == at => content.transform(c.a).map(k => c.copy(a = k))
      case _ => Some(a)
    }
  }
  case class Replace(at: cursor.Node, content: data.Content) extends Node {
    override def ty: Type = Type.AddDelete
    override def apply(d: data.Node): data.Node = {
      d.map(at, a => a.copy(content = content))
    }

    override def transform(a: mode.Node): Option[mode.Node] = a match {
      case c: mode.Node.Content if c.node == at => None
      case _ => Some(a)
    }
  }
  case class Insert(at: cursor.Node, childs: Seq[data.Node]) extends Node {
    override def ty: Type = Type.Add
    override def apply(d: data.Node): data.Node = d.insert(at, childs)

    override def transform(a: mode.Node): Option[mode.Node] = a match {
      case c: mode.Node.Content =>
        Some(c.modify(_.node).using(a => cursor.Node.transformAfterInserted(at, childs.size, a)))
      case mode.Node.Visual(fix, move) =>
        Some(mode.Node.Visual(
          cursor.Node.transformAfterInserted(at, childs.size, fix),
          cursor.Node.transformAfterInserted(at, childs.size, move)))
    }
  }
  case class Delete(r: range.Node) extends Node {
    override def ty: Type = Type.AddDelete
    override def apply(d: data.Node): data.Node = d.delete(r)

    override def transform(a: mode.Node): Option[mode.Node] = a match {
      case c@mode.Node.Content(node, _) => r.transformAfterDeleted(node) match {
        case Some(k) => Some(c.copy(node = k))
        case None => None
      }
      case mode.Node.Visual(fix, move) =>
        (r.transformAfterDeleted(fix), r.transformAfterDeleted(move)) match {
          case (Some(ff), Some(mm)) => Some(mode.Node.Visual(ff, mm))
          case _ => None
        }
    }
  }
  case class Move(r: range.Node, to: cursor.Node) extends Node {
    assert(!r.contains(to))
    override def ty: Type = Type.Structural
    override def apply(d: data.Node): data.Node = d.move(r, to)

    override def transform(a: mode.Node): Option[mode.Node] = Some(a match {
      case mode.Node.Visual(fix, move) => mode.Node.Visual(r.transformNodeAfterMoved(to, fix), r.transformNodeAfterMoved(to, move))
      case mode.Node.Content(node, b) => mode.Node.Content(r.transformNodeAfterMoved(to, node), b)
    })
  }

  override val pickler: Pickler[Node] = new Pickler[Node] {
    override def pickle(obj: Node)(implicit state: PickleState): Unit = {
      import state.enc._
      obj match {
        case Content(c, content) =>
          writeInt(0)
          writeIntArray(c.toArray)
          operation.Content.pickler.pickle(content)
        case Replace(c, content) =>
          writeInt(1)
          writeIntArray(c.toArray)
          data.Content.pickler.pickle(content)
        case Insert(c, childs) =>
          writeInt(2)
          writeIntArray(c.toArray)
          writeInt(childs.size)
          childs.foreach(a => data.Node.pickler.pickle(a))
        case Delete(r) =>
          writeInt(3)
          range.Node.pickler.pickle(r)
        case Move(r, a) =>
          writeInt(4)
          range.Node.pickler.pickle(r)
          writeIntArray(a.toArray)
      }
    }

    override def unpickle(implicit state: UnpickleState): Node = {
      import state.dec._
      readInt match {
        case 0 =>
          Content(readIntArray, operation.Content.pickler.unpickle)
        case 1 =>
          Replace(readIntArray, data.Content.pickler.unpickle)
        case 2 =>
          Insert(readIntArray, (0 until readInt).map(_ => data.Node.pickler.unpickle))
        case 3 =>
          Delete(range.Node.pickler.unpickle)
        case 4 =>
          Move(range.Node.pickler.unpickle, readIntArray)
      }
    }
  }

  def randomCursorAndNode(d: data.Node, r: Random): (cursor.Node, data.Node) = {
    if (d.childs.isEmpty || r.nextInt(3) == 0) {
      (cursor.Node.root, d)
    } else {
      val c = r.nextInt(d.childs.size)
      val (aa, bb) = randomCursorAndNode(d.childs(c), r)
      (c +: aa, bb)
    }
  }

  override def random(d: data.Node, r: Random): Node = {
    // LATER generate MOVE
    def doInsert(): Node = {
      val (c, n) = randomCursorAndNode(d, r)
      val p = r.nextInt(n.childs.size + 1)
      Insert(c :+ p, (0 until r.nextInt(3)).map(_ => data.Node.random(r)))
    }
    r.nextInt(4) match {
      case 0 =>
        val (c, n) = randomCursorAndNode(d, r)
        Content(c, operation.Content.random(n.content, r))
      case 1 =>
        val (c, _) = randomCursorAndNode(d, r)
        Replace(c, data.Content.random(r))
      case 2 =>
        val (c, _) = randomCursorAndNode(d, r)
        if (c != cursor.Node.root) {
          var ran = range.Node(c)
          if (r.nextBoolean() && ran.childs.start > 1) {
            ran = range.Node(ran.parent, ran.childs.modify(_.start).using(_ - 2))
          } else if (r.nextBoolean() && ran.childs.start > 0) {
            ran = range.Node(ran.parent, ran.childs.modify(_.start).using(_ - 1))
          }
          Delete(ran)
        } else {
          doInsert()
        }
      case 3 =>
        doInsert()
    }
  }
}
