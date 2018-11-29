package command.defaults

import client.Client
import command.{CommandCategory, CommandInterface, CommandInterfaceAvailable, Motion}
import command.Key.KeySeq
import doc.{DocState, DocTransaction}
import model.cursor.Node
import model.{cursor, data, mode, operation}
import model.data.{Node, Rich, Text, Unicode}
import model.operation.Node
import model.range.IntRange
import register.Registerable
import settings.Settings

class YankPaste(settings: Settings, isReadOnly: Boolean) extends CommandCategory(settings,"registers, yank and paste") {

  abstract class Command extends super.Command {
    override def showInCommandMenu(modal: Boolean): Boolean = false
    override def modalOnly: Boolean = true
  }

  new Command {
    override val description: String = "use register following for next delete, yank or put"
    override def defaultKeys: Seq[KeySeq] = Seq("\"")
    override def available(a: DocState, commandState: CommandInterfaceAvailable): Boolean = true
    override def needsChar: Boolean = true
    override def showInCommandMenu(modal: Boolean): Boolean = false

    override def action(a: DocState, count: Int, commandState: CommandInterface, key: Option[KeySeq], grapheme: Option[Unicode], motion: Option[Motion]): DocTransaction = {
      if (grapheme.nonEmpty) {
        val str = grapheme.get.str
        if (str.length == 1) {
          commandState.registers.setRegister(str.charAt(0))
        }
      }
      DocTransaction.empty
    }
  }

  new TextualCommand {
    override def modalOnly: Boolean = true
    override val description: String = "registers"
    override protected def available(a: DocState): Boolean = true

    override def textCommand: Seq[String] = Seq("reg")

    override def action(a: DocState, count: Int, commandState: CommandInterface, key: Option[KeySeq], grapheme: Option[Unicode], motion: Option[Motion]): DocTransaction = {
      DocTransaction.message(Client.ViewMessage.ShowRegisters())
    }
  }

  new Command {
    override val description: String = "yank current node (without childs)"
    override val defaultKeys: Seq[KeySeq] = Seq("yy")
    override def available(a: DocState): Boolean = a.isContent

    override protected def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      val c = a.asContent
      val node = a.node(c)
      commandState.registers.yank(Registerable.Node(Seq(node.copy(childs = Seq.empty))), isDelete = false)
      DocTransaction.empty
    }
  }

  new Command {
    override val description: String = "yank current node"
    override val defaultKeys: Seq[KeySeq] = Seq("yr") // siblings not lines
    override def available(a: DocState): Boolean = a.isContent

    override protected def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      val c = a.asContent
      commandState.registers.yank(Registerable.Node(Seq(a.node(c))), isDelete = false)
      DocTransaction.empty
    }
  }

  new Command {
    override val description: String = "yank to line end"
    override val defaultKeys: Seq[KeySeq] = Seq("Y")
    override def available(a: DocState): Boolean = a.isRichNonSub

    override protected def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      val (c, rich, normal) = a.asRich
      commandState.registers.yank(Registerable.Text(rich.copyTextualRange(IntRange(normal.merged.start, rich.size))), isDelete = false)
      DocTransaction.empty
    }
  }

  new Command {
    override val description: String = "yank selection"
    override val defaultKeys: Seq[KeySeq] = Seq("y")
    override def available(a: DocState): Boolean = a.isVisual || a.isCodeNormal

    override protected def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      yankSelection(a, commandState, settings.enableModal, false, -1)._1
    }
  }


  new Command {
    override val description: String = "yank range selected by motion"
    override def needsMotion: Boolean = true
    override val defaultKeys: Seq[KeySeq] = Seq("y")
    override protected def available(a: DocState): Boolean = a.isRichNormal
    override def repeatable: Boolean = true

    override def action(a: DocState, count: Int, commandState: CommandInterface, key: Option[KeySeq], grapheme: Option[Unicode], motion: Option[Motion]): DocTransaction = {
      val (cur, rich, normal) = a.asRichNormal
      if (rich.isEmpty) return DocTransaction.empty
      motion.foreach(m => {
        m.act(commandState, rich, count, normal.range, grapheme).foreach(r => {
          commandState.registers.yank(Registerable.Text(rich.copyTextualRange(r)), isDelete = false)
        })
      })
      DocTransaction.empty
    }
  }




  // CHECK PERMISSIONS
  abstract class PutCommand extends Command {
    override protected def available(a: DocState): Boolean = a.isContent

    def putNode(a: DocState, at: cursor.Node, node: Seq[data.Node]): (operation.Node.Insert, mode.Node)
    def select(selection: IntRange): Int
    override protected def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      val cursor = a.asContent
      def putTextInRich(n: Seq[Text]) = {
        def putBeforeAfter(rich: Rich, normal: IntRange) = {
          var canInsert = false
          val insertionPoint = select(normal)
          if (rich.insideCoded(insertionPoint)) {
            if (n.size == 1 && n.head.isPlain) {
              canInsert = true
            }
          } else {
            canInsert = true
          }
          // LATER insert serialized format
            val frag = if (canInsert) n else Seq(Text.Plain(Unicode({
              val pp = Text.toPlain(n)
              if (pp.isEmpty) " " else pp
            })))
          val op = Seq(operation.Rich.insert(insertionPoint, frag))
          val rg = Rich(frag).rangeEnd.moveBy(insertionPoint)
          val mode = model.mode.Content.RichNormal(rg).collapse(settings.enableModal)
          DocTransaction(operation.Node.rich(cursor, op), Some(a.copyContentMode(mode)))
        }
        if (n.nonEmpty) {
          def byMode(amode: model.mode.Content): DocTransaction = {
            amode match {
              case model.mode.Content.RichNormal(ran) =>
                putBeforeAfter(a.rich(cursor), ran)
              case model.mode.Content.RichInsert(i) =>
                putBeforeAfter(a.rich(cursor), IntRange(i, i))
              case v: model.mode.Content.RichRange =>
                val trans = deleteRichNormalRange(a, commandState, cursor, v.merged, insert = true, noHistory = true)
                val mode = if (trans.transaction.isEmpty) a.mode0 else trans.mode.get
                val t2 = byMode(mode.asInstanceOf[model.mode.Node.Content].a)
                t2.copy(transaction = trans.transaction ++ t2.transaction)
              case _ =>
                DocTransaction.empty
            }
          }
          byMode(a.contentMode)
        } else {
          DocTransaction.empty
        }
      }
      commandState.registers.retrieveSetRegisterAndSetToCloneNode(true) match {
        case Some(Registerable.Node(n, range)) =>
          if (n.nonEmpty) {
            val (insertOp, mode) = putNode(a, cursor, if (range.isEmpty) data.Node.cloneNodes(n) else n)
            val pCur = model.cursor.Node.parent(insertOp.at)
            val ctrans = changeHeadingLevel(insertOp.childs, model.range.Node(insertOp.at, insertOp.childs.length), pCur, a.node(pCur))
            DocTransaction(insertOp +: ctrans, Some(mode), tryMergeInsertOfDeleteRange = range)
          } else {
            DocTransaction.empty
          }
        case Some(Registerable.Text(n)) =>
          putTextInRich(n)
        case Some(Registerable.Unicode(n)) =>
          putTextInRich(Seq(Text.Plain(n)))
        case None => DocTransaction.empty
      }
    }
  }


  val putAfter: PutCommand = if (isReadOnly) null else  new PutCommand {
    override val description: String = "put after"
    override def defaultKeys: Seq[KeySeq] = Seq("p")


    override def select(selection: IntRange): Int = selection.until

    def putNode(a: DocState, at: cursor.Node, node: Seq[data.Node]): (operation.Node.Insert, mode.Node) = {
      val (insertionPoint, _, _) = insertPointAfter(a, at)
      (operation.Node.Insert(insertionPoint, node), mode.Node.Content(insertionPoint, node.head.content.defaultMode(settings.enableModal)))
    }

  }

  val putBefore : PutCommand = if (isReadOnly) null else new PutCommand {
    override val description: String = "put before"
    override def defaultKeys: Seq[KeySeq] = Seq("P")

    def putNode(a: DocState, at: cursor.Node, node: Seq[data.Node]): (operation.Node.Insert, mode.Node) = {
      val pt = if (at == a.zoom) a.zoom :+ 0 else at
      (operation.Node.Insert(pt, node), mode.Node.Content(pt, node.head.content.defaultMode(settings.enableModal)))
    }

    override def select(selection: IntRange): Int = selection.start
  }
}
