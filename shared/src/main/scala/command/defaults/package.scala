package command

import doc.{DocState, DocTransaction}
import model.data.{Node, NodeType}
import model.operation.Node.AttributeChange
import model.{cursor, data, mode, operation, range}
import model.range.IntRange
import register.Registerable

import scala.collection.mutable.ArrayBuffer

package object defaults {

  def changeHeadingLevel(nodes: Seq[data.Node], r: range.Node, parent: cursor.Node, parentNode: data.Node): Seq[operation.Node] = {
    var error = false
    val bf = new ArrayBuffer[operation.Node]()
    def rec(nodes: Seq[data.Node], r: range.Node, p: Int): Unit = {
      if (error || p >= 6) {
        error = true
      } else {
        nodes.zipWithIndex.filter(_._1.heading.exists(a => a != 1 && a != p + 1)).foreach(n => {
          val cur = r.parent :+ (r.childs.start + n._2)
          bf.append(operation.Node.AttributeChange(cur, data.Node.HeadingLevel, Some(p + 1)))
          if (n._1.childs.exists(_.heading.exists(_ != 1))) {
            rec(n._1.childs, range.Node(cur, IntRange(0, n._1.childs.size)), p + 1)
          }
        })
      }
    }
    parentNode.heading match {
      case Some(l) =>
        rec(nodes, r, l)
      case _ =>
    }
    if (error) {
      Seq.empty
    } else {
      bf
    }
  }

  def changeHeadingLevel(a: DocState, r: range.Node, parent: cursor.Node): Seq[operation.Node] = {
    changeHeadingLevel(a.node(r), r, parent, a.node(parent))
  }


  def yankSelection(a: DocState,
    commandState: CommandInterface,
    enableModal: Boolean,
    isDelete: Boolean,
    reg: Int = -1): (DocTransaction, Option[Registerable]) = {
    a.mode match {
      case Some(v@model.mode.Node.Visual(fix, _)) =>
        val ns = v.minimalRange match {
          case Some(k) => a.node(k)
          case _ =>
            if (isDelete) return (DocTransaction.empty, None)
            Seq(a.node)
        }
        val data = Registerable.Node(ns)
        commandState.registers.yank(data, isDelete = false, register = reg)
        val trans = if (isDelete) {
          deleteNodeRange(a, commandState, v.minimalRange.get, enableModal, noHistory = true)
        } else {
          DocTransaction(model.mode.Node.Content(fix, a.node(fix).content.defaultMode(enableModal)))
        }
        (trans, Some(data))
      case Some(model.mode.Node.Content(pos, v: model.mode.Content.RichRange)) =>
        val data = Registerable.Text(a.rich(pos).copyTextualRange(v.merged))
        commandState.registers.yank(data, isDelete = isDelete, register = reg)
        val trans = if (isDelete) {
          deleteRichNormalRange(a, commandState, pos, v.merged, !enableModal, noHistory = true)
        } else {
          if (enableModal) {
            DocTransaction(model.mode.Node.Content(pos, v.asInstanceOf[model.mode.Content.RichVisual].collapse))
          } else {
            DocTransaction.empty
          }
        }
        (trans, Some(data))
      case Some(model.mode.Node.Content(pos, v@model.mode.Content.CodeNormal(_))) =>
        val old = a.node(pos)
        val data = Registerable.Node(Seq(old.copy(childs = Seq.empty)), None)
        commandState.registers.yank(data, isDelete = false, register = reg)
        val trans = if (isDelete) {
          DocTransaction(Seq(model.operation.Node.Replace(pos,
            model.data.Content.Code(model.data.Unicode.empty, old.content.asInstanceOf[model.data.Content.Code].lang))), a.mode)
        } else {
          DocTransaction.empty
        }
        (trans, Some(data))
      case _ => throw new IllegalArgumentException("Invalid command")
    }
  }

  def deleteNodeRange(a: DocState,
    commandState: CommandInterface,
    rr: range.Node,
    enableModal: Boolean,
    register: Int = -1,
    noHistory: Boolean = false,
    goUp: Boolean = false): DocTransaction = {
    val parent = a.node(rr.parent)
    assert(rr.childs.until <= parent.childs.size)
    val r = rr
    if (!noHistory) {
      commandState.registers.yank(Registerable.Node(a.node(rr), deletionFrom = Some(rr)), isDelete = true, register = register)
    }
    DocTransaction(Seq(operation.Node.Delete(r)), {
      val (nowPos, toPos) =
//        if (goUp) {
//          a.mover().visualUp(rr.start).
//        } else {
//        }
      if (a.node.get(r.until).isDefined) {
        (r.until, r.start)
      } else if (r.childs.start > 0) {
        val p = r.parent :+ (r.childs.start - 1)
        (p, p)
      } else {
        (r.parent, r.parent)
      }
      Some(model.mode.Node.Content(toPos, a.node(nowPos).content.defaultMode(enableModal)))
    }, tryMergeDeletes = true)
  }

  def deleteRichNormalRange(a: DocState,
    commandState: CommandInterface,
    pos: cursor.Node,
    r: IntRange,
    insert: Boolean,
    noHistory: Boolean = false,
    register: Int = -1
  ): DocTransaction = {
    val rich = a.rich(pos)
    if (!noHistory) {
      commandState.registers.yank(Registerable.Text(rich.copyTextualRange(r)), isDelete = true, register = register)
    }
    operation.Rich.deleteTextualRange(rich, r) match {
      case Some((a, b, c)) =>
        DocTransaction(
          a.map(r => operation.Node.rich(pos, r)),
          Some(mode.Node.Content(pos,
            if (insert)
              mode.Content.RichInsert(if (c == 0) b.start else b.until)
            else
              mode.Content.RichNormal(b)
          ))
        )
      case None => DocTransaction.empty
    }
  }


  private[defaults] def insertPointAfter(a: DocState, pos: cursor.Node): (cursor.Node, Option[NodeType], Option[Int]) = {
    val mover = a.mover()
    val node = a.node(pos)
    if (pos == a.zoom || (node.isHeading && !a.viewAsFolded(pos))) {
      (pos :+ 0, None, None)
    } else {
      val cursor = mover.firstChild(pos).getOrElse(mover.nextOver(pos))
      if (a.nodeType(pos).shouldFollow) (cursor, node.attribute(data.NodeType), node.heading)
      else (cursor, None, None)
    }
  }

}
