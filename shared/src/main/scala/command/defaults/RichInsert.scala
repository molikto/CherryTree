package command.defaults

import client.Client
import command.CommandCollector
import command.Key._
import model.{ClientState, mode, operation}
import model.data.{Rich, SpecialChar}
import model.range.IntRange

trait RichInsert extends CommandCollector {


  trait EditCommand extends Command  {
    def edit(content: Rich,a: Int): Option[model.operation.Rich]
    override def available(a: ClientState): Boolean = a.isRichInserting
    override def action(a: ClientState, count: Int): Client.Update = {
      val (n, content, insert) = a.asRichInsert
      val res = edit(content, insert.pos).map(k => {
        Seq(model.operation.Node.Content(n, model.operation.Content.Rich(k)))
      }).getOrElse(Seq.empty)
      Client.Update(res, None)
    }
  }
  val backspace: Command = new EditCommand with OverrideCommand {
    // TODO these keys should be seperate delete words, etc...
    override val hardcodeKeys: Seq[KeySeq] = Backspace.withAllModifers ++ (if (model.isMac) Seq(Ctrl + "h") else Seq.empty[KeySeq])
    override def edit(content: Rich, a: Int): Option[operation.Rich] = {
      if (a > 0) {
        Some(operation.Rich.deleteOrUnwrapAt(content, a - 1)) // we don't explicitly set mode, as insert mode transformation is always correct
      } else {
        None
      }
    }
  }

  new OverrideCommand {
    // TODO what to do on enter???
    override val hardcodeKeys: Seq[KeySeq] = Enter.withAllModifers
    override def available(a: ClientState): Boolean = a.isRichInserting
    override def action(a: ClientState, count: Int): Client.Update = {
      val (node, rich, insert) =  a.asRichInsert
      if (insert.pos == rich.size) {
        val n = a.mover().nextOver(node)
        Client.Update(
          Seq(operation.Node.Insert(n, Seq(model.data.Node.empty)))
          , Some(model.mode.Node.Content(n, model.mode.Content.RichNormal(IntRange(0, 0)))))
      } else {
        Client.Update.empty
      }
    }
  }

  val emptyWraps: Map[SpecialChar.Delimitation, Command] = SpecialChar.all.map(deli => deli -> new DeliCommand(deli) {
    override def available(a: ClientState): Boolean = a.isRichInserting && {
      val (node, rich, insert) = a.asRichInsert
      !rich.insertionInsideCoded(insert.pos)
    }
    override def action(a: ClientState, count: Int): Client.Update = {
      val (n, content, insert) = a.asRichInsert
      val res = if (insert.pos < content.size && content.info(insert.pos).isSpecialChar(deli.end)) {
        Seq.empty
      } else {
        val k = operation.Rich.insert(insert.pos, deli.wrap())
        Seq(model.operation.Node.Content(n, model.operation.Content.Rich(k)))
      }
      Client.Update(res, Some(a.copyContentMode(mode.Content.RichInsert(insert.pos + 1))))
    }
  }).toMap

  abstract class InsertMovementCommand extends Command {
    override def available(a: ClientState): Boolean = a.isRichInserting
    def move(rich: Rich, i: Int): Int
    override def action(a: ClientState, count: Int): Client.Update = a.asRichInsert match {
      case (node, rich, insert) =>
        val m = move(rich, insert.pos)
        if (m != insert.pos) Client.Update.mode(a.copyContentMode(mode.Content.RichInsert(m))) else Client.Update.empty
    }
  }

  // LATER  insert movement
  // i_<S-Left>    shift-left/right  one word left/right
  //i_<S-Up>      shift-up/down     one screenful backward/forward
  //i_<End>       <End>             cursor after last character in the line
  //i_<Home>      <Home>            cursor to first character in the line
  val moveRight: Command = new InsertMovementCommand { // DIFFERENCE we added two move, also disabled up/down
    override def defaultKeys: Seq[KeySeq] = Seq(Alt + " ", Right)
    override def move(rich: Rich, i: Int): Int = rich.moveRightAtomic(i - 1).until
  }

  val moveLeft: Command = new InsertMovementCommand {
    override def defaultKeys: Seq[KeySeq] = Seq(Left)
    override def move(rich: Rich, i: Int): Int = rich.moveLeftAtomic(i).start
  }

  val disableUp: Command = new InsertMovementCommand with OverrideCommand {
    override def defaultKeys: Seq[KeySeq] = Seq(Up)
    override def move(rich: Rich, i: Int): Int = i
  }

  val disableDown: Command = new InsertMovementCommand with OverrideCommand {
    override def defaultKeys: Seq[KeySeq] = Seq(Down)
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
