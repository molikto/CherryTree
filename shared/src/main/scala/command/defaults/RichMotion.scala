package command.defaults

import client.Client
import command.{CommandCategory, CommandInterface, CommandInterfaceAvailable, Motion}
import model.range.IntRange
import command.Key._
import doc.{DocState, DocTransaction}
import model.data.{Rich, Unicode}
import settings.Settings

class RichMotion extends CommandCategory("rich text: cursor motion") {



  trait RichMotionCommand extends Command with command.RichMotion {
    override def available(a: DocState, commandState: CommandInterfaceAvailable): Boolean = a.isRichNormalOrVisual || commandState.needsMotion
  }


  trait InsertRichMotionCommand extends Command with command.RichMotion {
    override def available(a: DocState, commandState: CommandInterfaceAvailable): Boolean = a.isRich || commandState.needsMotion
  }

  abstract class SimpleRichMotionCommand extends RichMotionCommand {

    override def repeatable: Boolean = true

    final override def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      val (_, content, m) = a.asRich
      if (content.isEmpty) {
        DocTransaction.empty
      } else {
        def act(r: IntRange) = (0 until count).foldLeft(r) { (rr, _) => move(content, rr)._1 }
        DocTransaction(a.copyContentMode(m.copyWithNewFocus(act(m.merged), enableModal)))
      }
    }
  }


  new SimpleRichMotionCommand() with InsertRichMotionCommand { // DIFFERENCE h + Control is also in Vim, but we don't use this,
    override val description: String = "move left"
    override val hardcodeKeys: Seq[KeySeq] = Seq(Left)
    override val defaultKeys = Seq("h", Backspace)
    override def move(content: Rich, a: IntRange): (IntRange, Int) = if (a.start == 0) (a, 0) else (content.before(a.start).range, 0)
  }

  new SimpleRichMotionCommand() with InsertRichMotionCommand {
    override val description: String = "move right"
    override val hardcodeKeys: Seq[KeySeq] = Seq(Right)
    override val defaultKeys = Seq("l")  // DIFFERENCE space is for smart move
    override def move(content: Rich, a: IntRange): (IntRange, Int)  =  if (a.until == content.size) (a, 1) else (content.after(a.until).range, 0)
  }

  new SimpleRichMotionCommand() with InsertRichMotionCommand {
    override def repeatable: Boolean = false
    override val description: String = "move to beginning"
    override val defaultKeys = Seq("0", "^", Home) // DIFFERENCE merged because we are already structural
    override def move(content: Rich, a: IntRange): (IntRange, Int) = (content.rangeBeginning, 0)
  }

  new SimpleRichMotionCommand with InsertRichMotionCommand {
    override def repeatable: Boolean = false
    override val description: String = "move to end"
    override val defaultKeys = Seq("$", End) // DIFFERENCE is not repeatable, different from Vim
    override def move(content: Rich, a: IntRange):  (IntRange, Int) = (content.rangeEnd, 1)
    override def priority: Int = 1 // override LaTeX
  }

  // screen related is not implemented
  // LATER
  //g0       g0           to first character in screen line (differs from "0"
  //                           when lines wrap)
  //g^       g^           to first non-blank character in screen line (differs
  //                           from "^" when lines wrap)
  //g$    N  g$           to last character in screen line (differs from "$"
  //                           when lines wrap)
  //gm       gm           to middle of the screen line

  // not implemented...?? because it is hard to make columns in a rich text editor
  // bar   N  |            to column N (default: 1)

  abstract class FindCommand extends RichMotionCommand with NeedsCharCommand with command.FindCommand {

    override def settings: Settings = RichMotion.this

    protected def move(a: Rich, range: IntRange, char: Unicode): Option[IntRange]
    def skip(a: Rich, range: IntRange): IntRange = range
    def moveSkip(a: Rich, range: IntRange, char: Unicode, skipCurrent: Boolean): Option[IntRange] = {
      if (skipCurrent) {
        move(a, range, char).flatMap(m => {
          if (m == range) {
            move(a, skip(a, m), char)
          } else {
            Some(m)
          }
        })
      } else {
        move(a, range, char)
      }
    }

    override def findGrapheme(a: Rich, r: IntRange, char: Unicode, count: Int, skipCurrent: Boolean): Option[IntRange] = {
      (0 until count).foldLeft(Some(r): Option[IntRange]) {(r, i) =>
        r.flatMap(rr => moveSkip(a, rr, char, skipCurrent || i > 0))
      }
    }

    override def action(a: DocState, count: Int, commandState: CommandInterface, key: Option[KeySeq], grapheme: Option[Unicode], motion: Option[Motion]): DocTransaction = {
      if (grapheme.isEmpty) return DocTransaction.empty
      val char = grapheme.get
      action(a, char, count, skipCurrent = false)
    }

    override def repeatable: Boolean = true
  }

  val findNextChar: FindCommand = new FindCommand {
    override val description: String = "find char after cursor"
    override def reverse: FindCommand = findPreviousChar
    override val defaultKeys: Seq[KeySeq] = Seq("f")
    def move(a: Rich, range: IntRange, char: Unicode): Option[IntRange] = a.findCharAfter(range, char, delimitationGraphemes).map(_.range)
  }
  val findPreviousChar: FindCommand = new FindCommand {
    override val description: String = "find char before cursor"
    override def reverse: FindCommand = findNextChar
    override val defaultKeys: Seq[KeySeq] = Seq("F")
    def move(a: Rich, range: IntRange, char: Unicode): Option[IntRange] = a.findCharBefore(range, char, delimitationGraphemes).map(_.range)
  }
  val toNextChar: FindCommand = new FindCommand {
    override val description: String = "find char after cursor, move cursor before it"
    override def reverse: FindCommand = toPreviousChar
    override val defaultKeys: Seq[KeySeq] = Seq("t")
    override def skip(content: Rich, range: IntRange): IntRange = content.rangeAfter(range)
    override def move(content: Rich, range: IntRange, char: Unicode): Option[IntRange] =
      content.findCharAfter(range, char, delimitationGraphemes).map(r => content.rangeBefore(r.range))
  }
  val toPreviousChar: FindCommand = new FindCommand {
    override val description: String = "find char before cursor, move cursor after it"
    override def reverse: FindCommand = toNextChar
    override val defaultKeys: Seq[KeySeq] = Seq("T")
    override def skip(content: Rich, range: IntRange): IntRange = content.rangeBefore(range)
    override def move(content: Rich, range: IntRange, char: Unicode): Option[IntRange] =
      content.findCharBefore(range, char, delimitationGraphemes).map(r => content.rangeAfter(r.range))
  }



  new RichMotionCommand {
    override def repeatable: Boolean = true
    override val description: String = "repeat previous find command"
    override def available(a: DocState, commandState: CommandInterfaceAvailable): Boolean = super.available(a, commandState) && commandState.lastFindCommand.isDefined
    override val defaultKeys: Seq[KeySeq] = Seq(";")

    override def action(a: DocState, count: Int, commandState: CommandInterface, key: Option[KeySeq], grapheme: Option[Unicode], motion: Option[Motion]): DocTransaction = {
      val lf = commandState.lastFindCommand.get
      lf._1.action(a, lf._2, count, skipCurrent = true)
    }

    override def move(commandState: CommandInterface, content: Rich, count: Int, r: IntRange, char: Option[Unicode]): Option[(IntRange, Int)] = {
      val lf = commandState.lastFindCommand.get
      char.flatMap(c => lf._1.findGrapheme(content, r, c, count, skipCurrent = true)).map(a => (a, 1))
    }

    override protected def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = throw new IllegalStateException("No one should call this if the other implemented")

  }

  new RichMotionCommand {
    override def repeatable: Boolean = true
    override val description: String = "repeat previous find command's reverse"
    override def available(a: DocState, commandState: CommandInterfaceAvailable): Boolean = super.available(a, commandState) && commandState.lastFindCommand.isDefined
    override val defaultKeys: Seq[KeySeq] = Seq(",")
    override def action(a: DocState, count: Int, commandState: CommandInterface, key: Option[KeySeq], grapheme: Option[Unicode], motion: Option[Motion]): DocTransaction = {
      val lf = commandState.lastFindCommand.get
      lf._1.reverse.action(a, lf._2, count, skipCurrent = true)
    }
    override protected def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = throw new IllegalStateException("No one should call this if the other implemented")

    override def move(commandState: CommandInterface, content: Rich, count: Int, r: IntRange, char: Option[Unicode]): Option[(IntRange, Int)] = {
      val lf = commandState.lastFindCommand.get
      char.flatMap(c => lf._1.reverse.findGrapheme(content, r, c, count, skipCurrent = true)).map(a => (a, 1))
    }
  }

  new SimpleRichMotionCommand {
    override val description: String = "forward by word"
    override val defaultKeys: Seq[KeySeq] = Seq("w")
    override def move(content: Rich, a: IntRange): (IntRange, Int) = content.moveRightWord(a).map(a => (a, 0)).getOrElse((content.rangeEnd, 1))

  }
  new SimpleRichMotionCommand {
    override val description: String = "forward by WORD"
    override val defaultKeys: Seq[KeySeq] = Seq("W")
    override def move(content: Rich, a: IntRange): (IntRange, Int) = content.moveRightWORD(a).map(a => (a, 0)).getOrElse((content.rangeEnd, 1))
  }
  new SimpleRichMotionCommand {
    override val description: String = "backward by word"
    override val defaultKeys: Seq[KeySeq] = Seq("b")
    override def move(content: Rich, a: IntRange):  (IntRange, Int)  = content.moveLeftWord(a).map(a => (a, 0)).getOrElse((content.rangeBeginning, 1))
  }

  new SimpleRichMotionCommand {
    override val description: String = "backward by WORD"
    override val defaultKeys: Seq[KeySeq] = Seq("B")
    override def move(content: Rich, a: IntRange):  (IntRange, Int)  =content.moveLeftWORD(a).map(a => (a, 0)).getOrElse((content.rangeBeginning, 1))
  }

  new SimpleRichMotionCommand {
    override val description: String = "forward to word end"
    override val defaultKeys: Seq[KeySeq] = Seq("e")
    override def move(content: Rich, a: IntRange):  (IntRange, Int) = content.moveRightWordEnd(a).map(a => (a, 1)).getOrElse((content.rangeEnd, 1))
  }

  new SimpleRichMotionCommand {
    override val description: String = "forward to WORD end"
    override val defaultKeys: Seq[KeySeq] = Seq("E")
    override def move(content: Rich, a: IntRange):  (IntRange, Int) = content.moveRightWORDEnd(a).map(a => (a, 1)).getOrElse((content.rangeEnd, 1))
  }

  new SimpleRichMotionCommand {
    override val description: String = "backward to word end"
    override val defaultKeys: Seq[KeySeq] = Seq("ge")
    override def move(content: Rich, a: IntRange):  (IntRange, Int)  =content.moveLeftWordEnd(a).map(a => (a, 0)).getOrElse((content.rangeBeginning, 1))
  }

  new SimpleRichMotionCommand {
    override val description: String = "backward to WORD end"
    override val defaultKeys: Seq[KeySeq] = Seq("gE")
    override def move(content: Rich, a: IntRange):  (IntRange, Int)  =content.moveLeftWORDEnd(a).map(a => (a, 0)).getOrElse((content.rangeBeginning, 1))
  }

  // LATER
  //        )     N  )            N sentences forward
  //        (     N  (            N sentences backward




  // not implemented because we are already a structured editor
  //      }     N  }            N paragraphs forward
  //      {     N  {            N paragraphs backward
  //        ]]    N  ]]           N sections forward, at start of section
  //        [[    N  [[           N sections backward, at start of section
  //          ][    N  ][           N sections forward, at end of section
  //          []    N  []           N sections backward, at end of section

  // not implemented for code related stuff
  //          [(    N  [(           N times back to unclosed '('
  //        [{    N  [{           N times back to unclosed '{'
  //          [m    N  [m           N times back to start of method (for Java)
  //          [M    N  [M           N times back to end of method (for Java)
  //          ])    N  ])           N times forward to unclosed ')'
  //          ]}    N  ]}           N times forward to unclosed '}'
  //        ]m    N  ]m           N times forward to start of method (for Java)
  //        ]M    N  ]M           N times forward to end of method (for Java)
  //          [#    N  [#           N times back to unclosed "#if" or "#else"
  //        ]#    N  ]#           N times forward to unclosed "#else" or "#endif"
  //        [star N  [*           N times back to start of comment "/*"
  //          ]star N  ]*           N times forward to end of comment "*/"


  // text objects not implemented
  // v_ap     N  ap        Select "a paragraph"
  //v_ip     N  ip        Select "inner paragraph"
  //v_ab     N  ab        Select "a block" (from "[(" to "])")
  //v_ib     N  ib        Select "inner block" (from "[(" to "])")
  //v_aB     N  aB        Select "a Block" (from "[{" to "]}")
  //v_iB     N  iB        Select "inner Block" (from "[{" to "]}")
  //v_a>     N  a>        Select "a <> block"
  //v_i>     N  i>        Select "inner <> block"
  //v_at     N  at        Select "a tag block" (from <aaa> to </aaa>)
  //v_it     N  it        Select "inner tag block" (from <aaa> to </aaa>)

}
