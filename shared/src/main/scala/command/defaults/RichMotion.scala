package command.defaults

import client.Client
import command.{CommandCategory, CommandState}
import model.range.IntRange
import command.Key._
import doc.{DocState, DocTransaction}
import model.data.{Rich, Unicode}

class RichMotion extends CommandCategory("move cursor inside text") {


  abstract class RichMotionCommand extends MotionCommand {

    override def repeatable: Boolean = true

    def move(content: model.data.Rich, a: IntRange): IntRange

    final override def action(a: DocState, count: Int): DocTransaction = {
      val (_, content, m) = a.asRichNormalOrVisual
      def act(r: IntRange) = (0 until count).foldLeft(r) { (rr, _) => move(content, rr) }
      DocTransaction.mode(a.copyContentMode(m.copyWithNewFocus(act(m.focus))))
    }
  }


  new RichMotionCommand() { // DIFFERENCE h + Control is also in Vim, but we don't use this,
    override val description: String = "move left"
    override val defaultKeys = Seq("h", Backspace, Left)
    override def move(content: Rich, a: IntRange): IntRange = content.rangeBefore(a)
  }

  new RichMotionCommand() {
    override val description: String = "move right"
    override val defaultKeys = Seq("l", Right)  // DIFFERENCE space is for smart move
    override def move(content: Rich, a: IntRange): IntRange = content.rangeAfter(a)
  }

  new RichMotionCommand() {
    override def repeatable: Boolean = false
    override val description: String = "move to beginning"
    override val defaultKeys = Seq("0", "^", Home) // DIFFERENCE merged because we are already structural
    override def move(content: Rich, a: IntRange): IntRange = content.rangeBeginning
  }

  new RichMotionCommand {
    override def repeatable: Boolean = false
    override val description: String = "move to end"
    override val defaultKeys = Seq("$", End) // DIFFERENCE is not repeatable, different from Vim
    override def move(content: Rich, a: IntRange): IntRange = content.rangeEnd
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

  abstract class FindCommand extends MotionCommand with NeedsCharCommand with command.FindCommand {

    def move(a: Rich, range: IntRange, char: Unicode): Option[IntRange]
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

    override def findGrapheme(a: DocState, char: Unicode, count: Int, skipCurrent: Boolean): DocTransaction = {
      val (_, content, mm) = a.asRichNormalOrVisual
      def act(r: IntRange) = (0 until count).foldLeft(Some(r): Option[IntRange]) {(r, i) =>
        r.flatMap(rr => moveSkip(content, rr, char, skipCurrent || i > 0))
      }
      act(mm.focus) match {
        case Some(move) =>
          DocTransaction.mode(a.copyContentMode(mm.copyWithNewFocus(move)))
        case None =>
          DocTransaction.empty
      }
    }

    final override def actionOnGrapheme(a: DocState, char: Unicode, count: Int): DocTransaction = {
      findGrapheme(a, char, count, skipCurrent = false)
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
    override val description: String = "find char after cursor, move cursor after it"
    override def reverse: FindCommand = toNextChar
    override val defaultKeys: Seq[KeySeq] = Seq("T")
    override def skip(content: Rich, range: IntRange): IntRange = content.rangeBefore(range)
    override def move(content: Rich, range: IntRange, char: Unicode): Option[IntRange] =
      content.findCharBefore(range, char, delimitationGraphemes).map(r => content.rangeAfter(r.range))
  }



  new MotionCommand {
    override def repeatable: Boolean = true
    override val description: String = "repeat previous find command"
    override def available(a: DocState, commandState: CommandState): Boolean = super.available(a) && commandState.lastFindCommand.isDefined
    override val defaultKeys: Seq[KeySeq] = Seq(";")
    override def action(a: DocState, count: Int, commandState: CommandState, key: Option[KeySeq]): DocTransaction = {
      val lf = commandState.lastFindCommand.get
      lf._1.findGrapheme(a, lf._2, count, skipCurrent = true)
    }
    override protected def action(a: DocState, count: Int): DocTransaction = throw new IllegalStateException("No one should call this if the other implemented")

  }

  new MotionCommand {
    override def repeatable: Boolean = true
    override val description: String = "repeat previous find command's reverse"
    override def available(a: DocState, commandState: CommandState): Boolean = super.available(a) && commandState.lastFindCommand.isDefined
    override val defaultKeys: Seq[KeySeq] = Seq(",")
    override def action(a: DocState, count: Int, commandState: CommandState, key: Option[KeySeq]): DocTransaction = {
      val lf = commandState.lastFindCommand.get
      lf._1.reverse.findGrapheme(a, lf._2, count, skipCurrent = true)
    }
    override protected def action(a: DocState, count: Int): DocTransaction = throw new IllegalStateException("No one should call this if the other implemented")
  }

  new RichMotionCommand {
    override val description: String = "forward by word"
    override val defaultKeys: Seq[KeySeq] = Seq("w")
    override def move(content: Rich, a: IntRange): IntRange = content.moveRightWord(a)
  }
  new RichMotionCommand {
    override val description: String = "forward by WORD"
    override val defaultKeys: Seq[KeySeq] = Seq("W")
    override def move(content: Rich, a: IntRange): IntRange = content.moveRightWORD(a)
  }
  new RichMotionCommand {
    override val description: String = "backward by word"
    override val defaultKeys: Seq[KeySeq] = Seq("b")
    override def move(content: Rich, a: IntRange): IntRange = content.moveLeftWord(a)
  }

  new RichMotionCommand {
    override val description: String = "backward by WORD"
    override val defaultKeys: Seq[KeySeq] = Seq("B")
    override def move(content: Rich, a: IntRange): IntRange = content.moveLeftWORD(a)
  }

  new RichMotionCommand {
    override val description: String = "forward to the end of next word"
    override val defaultKeys: Seq[KeySeq] = Seq("e")
    override def move(content: Rich, a: IntRange): IntRange = content.moveRightWordEnd(a)
  }

  new RichMotionCommand {
    override val description: String = "forward to the end of next WORD"
    override val defaultKeys: Seq[KeySeq] = Seq("E")
    override def move(content: Rich, a: IntRange): IntRange = content.moveRightWORDEnd(a)
  }

  new RichMotionCommand {
    override val description: String = "backward to the end of next word"
    override val defaultKeys: Seq[KeySeq] = Seq("ge")
    override def move(content: Rich, a: IntRange): IntRange = content.moveLeftWordEnd(a)
  }

  new RichMotionCommand {
    override val description: String = "backward to the end of next WORD"
    override val defaultKeys: Seq[KeySeq] = Seq("gE")
    override def move(content: Rich, a: IntRange): IntRange = content.moveLeftWORDEnd(a)
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



}
