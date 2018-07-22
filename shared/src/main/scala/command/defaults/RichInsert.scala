package command.defaults

import client.Client
import command.CommandCategory
import command.Key._
import doc.{DocState, DocTransaction}
import model.{mode, operation}
import model.data.{Rich, SpecialChar}
import model.range.IntRange

class RichInsert extends CommandCategory("when in insert mode") {


  trait EditCommand extends Command  {
    def edit(content: Rich,a: Int): Option[model.operation.Rich]
    override def available(a: DocState): Boolean = a.isRichInserting
    override def action(a: DocState, count: Int): DocTransaction = {
      val (n, content, insert) = a.asRichInsert
      val res = edit(content, insert.pos).map(k => {
        Seq(model.operation.Node.Content(n, model.operation.Content.Rich(k)))
      }).getOrElse(Seq.empty)
      DocTransaction(res, None)
    }
  }
  new EditCommand with OverrideCommand {
    override def description: String = "delete text before cursor"
    // TODO these keys should be seperate delete words, etc...
    override val hardcodeKeys: Seq[KeySeq] = (Backspace: KeySeq) +: (if (model.isMac) Seq(Ctrl + "h") else Seq.empty[KeySeq])
    override def edit(content: Rich, a: Int): Option[operation.Rich] = {
      if (a > 0) {
        Some(operation.Rich.deleteOrUnwrapAt(content, a - 1)) // we don't explicitly set mode, as insert mode transformation is always correct
      } else {
        None
      }
    }
  }

  new EditCommand with OverrideCommand {
    override def description: String = "delete text after cursor"
    // TODO these keys should be seperate delete words, etc...
    override val hardcodeKeys: Seq[KeySeq] = (Delete: KeySeq) +: (if (model.isMac) Seq(Ctrl + "d") else Seq.empty[KeySeq])
    override def edit(content: Rich, a: Int): Option[operation.Rich] = {
      if (a < content.size) {
        Some(operation.Rich.deleteOrUnwrapAt(content, a))
      } else {
        None
      }
    }
  }


  new OverrideCommand {
    override def description: String = "open a new sibling next to current one and continue in insert mode (currently only works when you are in end of text)"
    // TODO what to do on enter???
    override val hardcodeKeys: Seq[KeySeq] = Seq(Enter)
    override def available(a: DocState): Boolean = a.isRichInserting
    override def action(a: DocState, count: Int): DocTransaction = {
      val (node, rich, insert) =  a.asRichInsert
      if (insert.pos == rich.size) {
        val mover = a.mover()
        val n = mover.firstChild(node).getOrElse(mover.nextOver(node))
        DocTransaction(
          Seq(operation.Node.Insert(n, Seq(model.data.Node.empty)))
          , Some(model.mode.Node.Content(n, model.mode.Content.RichInsert(0))))
      } else {
        DocTransaction.empty
      }
    }
  }

  SpecialChar.all.map(deli => deli -> new DeliCommand(deli) {
    override def description: String = s"insert a new ${deli.name}"
    override def available(a: DocState): Boolean = a.isRichInserting && {
      val (node, rich, insert) = a.asRichInsert
      !rich.insertionInsideCoded(insert.pos)
    }
    override def action(a: DocState, count: Int): DocTransaction = {
      val (n, content, insert) = a.asRichInsert
      val res = if (insert.pos < content.size && content.info(insert.pos).isSpecialChar(deli.end)) {
        Seq.empty
      } else {
        val k = operation.Rich.insert(insert.pos, deli.wrap())
        Seq(model.operation.Node.Content(n, model.operation.Content.Rich(k)))
      }
      DocTransaction(res, Some(a.copyContentMode(mode.Content.RichInsert(insert.pos + 1))))
    }

  }).toMap

  abstract class InsertMovementCommand extends Command {
    override def available(a: DocState): Boolean = a.isRichInserting
    def move(rich: Rich, i: Int): Int
    override def action(a: DocState, count: Int): DocTransaction = a.asRichInsert match {
      case (node, rich, insert) =>
        val m = move(rich, insert.pos)
        if (m != insert.pos) DocTransaction.mode(a.copyContentMode(mode.Content.RichInsert(m))) else DocTransaction.empty
    }
  }

  // LATER  insert movement
  // i_<S-Left>    shift-left/right  one word left/right
  //i_<S-Up>      shift-up/down     one screenful backward/forward
  //i_<End>       <End>             cursor after last character in the line
  //i_<Home>      <Home>            cursor to first character in the line
  new InsertMovementCommand { // DIFFERENCE we added two move, also disabled up/down
    override def description: String = "move cursor right"
    override def defaultKeys: Seq[KeySeq] = Seq(Right)
    override def move(rich: Rich, i: Int): Int = rich.moveRightAtomic(i - 1).until
  }

  new InsertMovementCommand {
    override def description: String = "move cursor left"
    override def defaultKeys: Seq[KeySeq] = Seq(Left)
    override def move(rich: Rich, i: Int): Int = rich.moveLeftAtomic(i).start
  }

  // disabled keys!!!!
  new InsertMovementCommand with OverrideCommand {
    override def description: String = ""
    override def hardcodeKeys: Seq[KeySeq] = Seq[KeySeq](Down: KeySeq, Up)
    override def move(rich: Rich, i: Int): Int = i
  }

  // LATER insert movements
  // moving around:
  //i_<Up>        cursor keys       move cursor left/right/up/down
  //i_<S-Left>    shift-left/right  one word left/right
  //i_<S-Up>      shift-up/down     one screenful backward/forward
  //i_<End>       <End>             cursor after last character in the line
  //i_<Home>      <Home>            cursor to first character in the line

  // LATER seems all very wired
  // Q_ss          Special keys in Insert mode
  //
  //i_CTRL-V      CTRL-V {char}..   insert character literally, or enter decimal
  //                                     byte value
  //i_<NL>        <NL> or <CR> or CTRL-M or CTRL-J
  //                                  begin new line
  //i_CTRL-E      CTRL-E            insert the character from below the cursor
  //i_CTRL-Y      CTRL-Y            insert the character from above the cursor
  //
  //i_CTRL-A      CTRL-A            insert previously inserted text
  //i_CTRL-@      CTRL-@            insert previously inserted text and stop
  //                                     Insert mode
  //i_CTRL-R      CTRL-R {0-9a-z%#:.-="}  insert the contents of a register
  //
  //i_CTRL-N      CTRL-N            insert next match of identifier before the
  //                                     cursor
  //i_CTRL-P      CTRL-P            insert previous match of identifier before
  //                                     the cursor
  //i_CTRL-X      CTRL-X ...        complete the word before the cursor in
  //                                     various ways
  //
  //i_<BS>        <BS> or CTRL-H    delete the character before the cursor
  //i_<Del>       <Del>             delete the character under the cursor
  //i_CTRL-W      CTRL-W            delete word before the cursor
  //i_CTRL-U      CTRL-U            delete all entered characters in the current
  //                                     line
  //i_CTRL-T      CTRL-T            insert one shiftwidth of indent in front of
  //                                       the current line
  //i_CTRL-D      CTRL-D            delete one shiftwidth of indent in front of
  //                                     the current line
  //i_0_CTRL-D    0 CTRL-D          delete all indent in the current line
  //i_^_CTRL-D    ^ CTRL-D          delete all indent in the current line,
  //                                     restore indent in next line
}
