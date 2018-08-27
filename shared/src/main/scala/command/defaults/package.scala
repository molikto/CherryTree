package command

import doc.{DocState, DocTransaction}
import model.{cursor, data, mode, operation, range}
import model.range.IntRange
import register.Registerable

package object defaults {


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
        val data = Registerable.Node(ns, needsClone = true)
        commandState.yank(Registerable.Node(ns, needsClone = true), isDelete = false, register = reg)
        val trans = if (isDelete) {
          deleteNodeRange(a, commandState, v.minimalRange.get, enableModal, noHistory = true)
        } else {
          DocTransaction(model.mode.Node.Content(fix, a.node(fix).content.defaultMode(enableModal)))
        }
        (trans, Some(data))
      case Some(model.mode.Node.Content(pos, v@model.mode.Content.RichVisual(fix, _))) =>
        val data = Registerable.Text(a.rich(pos).copyTextualRange(v.merged))
        commandState.yank(data, isDelete = isDelete, register = reg)
        val trans = if (isDelete) {
          deleteRichNormalRange(a, commandState, pos, v.merged, enableModal, noHistory = true)
        } else {
          if (enableModal) {
            DocTransaction(model.mode.Node.Content(pos, model.mode.Content.RichNormal(fix)))
          } else {
            DocTransaction.empty
          }
        }
        (trans, Some(data))
      case Some(model.mode.Node.Content(pos, v@model.mode.Content.CodeNormal)) =>
        val data = Registerable.Unicode(a.node(pos).content.asInstanceOf[model.data.Content.Code].unicode)
        commandState.yank(data, isDelete = isDelete, register = reg)
        val trans = if (isDelete) {
          deleteNodeRange(a, commandState, model.range.Node(pos), enableModal, noHistory = true)
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
    val r = rr.copy(childs = IntRange(rr.childs.start, rr.childs.until min parent.childs.size))
    if (!noHistory) {
      commandState.yank(Registerable.Node(a.node(rr), from = Some(rr), needsClone = false), isDelete = true, register = register)
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
      commandState.yank(Registerable.Text(rich.copyTextualRange(r)), isDelete = true, register = register)
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


  private[defaults] def insertPointAfter(a: DocState, pos: cursor.Node): (cursor.Node, Option[data.Node.ContentType]) = {
    val mover = a.mover()
    val node = a.node(pos)
    if (pos == a.zoom || (node.isHeading && !a.viewAsFolded(pos))) {
      (pos :+ 0, None)
    } else {
      (mover.firstChild(pos).getOrElse(mover.nextOver(pos)), if (node.isHeading) node.attribute(data.Node.ContentType) else None)
    }
  }

}
