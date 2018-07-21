package command.defaults

import client.Client
import command.{CommandCategory, CommandState}
import model.range.IntRange
import command.Key._
import doc.{DocState, DocTransaction}
import model.data.Rich

class RichMotion extends CommandCategory("move cursor inside text") {


  // DIFFERENCE content motion is only available on paragraphs, editing of code is handled by third party editor!!!

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
    override def description: String = "move left"
    override val defaultKeys = Seq("h", Backspace, Left)
    override def move(content: Rich, a: IntRange): IntRange = content.moveLeftAtomic(a)
  }

  new RichMotionCommand() {
    override def description: String = "move right"
    override val defaultKeys = Seq("l", Right)  // DIFFERENCE space is for smart move
    override def move(content: Rich, a: IntRange): IntRange = content.moveRightAtomic(a)
  }

  new RichMotionCommand() {
    override def repeatable: Boolean = false
    override def description: String = "move to beginning"
    override val defaultKeys = Seq("0", "^", Home) // DIFFERENCE merged because we are already structural
    override def move(content: Rich, a: IntRange): IntRange = content.beginningAtomicRange()
  }

  new RichMotionCommand {
    override def repeatable: Boolean = false
    override def description: String = "move to end"
    override val defaultKeys = Seq("$", End) // DIFFERENCE is not repeatable, different from Vim
    override def move(content: Rich, a: IntRange): IntRange = content.endAtomicRange()
  }

  // screen related is not implemented
  //g0       g0           to first character in screen line (differs from "0"
  //                           when lines wrap)
  //g^       g^           to first non-blank character in screen line (differs
  //                           from "^" when lines wrap)
  //g$    N  g$           to last character in screen line (differs from "$"
  //                           when lines wrap)
  //gm       gm           to middle of the screen line

  // not implemented...?? because it is hard to make columns in a rich text editor
  // bar   N  |            to column N (default: 1)

  // TODO make find not side effecting
  abstract class FindCommand extends MotionCommand with NeedsCharCommand with command.FindCommand {

    def move(a: Rich, range: IntRange, char: Grapheme): Option[IntRange]
    def skip(a: Rich, range: IntRange): IntRange = range
    def moveSkip(a: Rich, range: IntRange, char: Grapheme, skipCurrent: Boolean): Option[IntRange] = {
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

    override def findGrapheme(a: DocState, char: Grapheme, count: Int, skipCurrent: Boolean): DocTransaction = {
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

    final override def actionOnGrapheme(a: DocState, char: Grapheme, count: Int): DocTransaction = {
      findGrapheme(a, char, count, skipCurrent = false)
    }

    override def repeatable: Boolean = true
  }

  val findNextChar: FindCommand = new FindCommand {
    override def description: String = "find char after cursor"
    override def reverse: FindCommand = findPreviousChar
    override val defaultKeys: Seq[KeySeq] = Seq("f")
    def move(a: Rich, range: IntRange, char: Grapheme): Option[IntRange] = a.findRightCharAtomic(range, char.a, delimitationCodePoints)

  }
  val findPreviousChar: FindCommand = new FindCommand {
    override def description: String = "find char before cursor"
    override def reverse: FindCommand = findNextChar
    override val defaultKeys: Seq[KeySeq] = Seq("F")
    def move(a: Rich, range: IntRange, char: Grapheme): Option[IntRange] = a.findLeftCharAtomic(range, char.a, delimitationCodePoints)
  }
  val toNextChar: FindCommand = new FindCommand {
    override def description: String = "find char after cursor, move cursor before it"
    override def reverse: FindCommand = toPreviousChar
    override val defaultKeys: Seq[KeySeq] = Seq("t")
    override def skip(content: Rich, range: IntRange): IntRange = content.moveRightAtomic(range)
    override def move(content: Rich, range: IntRange, char: Grapheme): Option[IntRange] =
      content.findRightCharAtomic(range, char.a, delimitationCodePoints).map(r => content.moveLeftAtomic(r))
  }
  val toPreviousChar: FindCommand = new FindCommand {
    override def description: String = "find char after cursor, move cursor after it"
    override def reverse: FindCommand = toNextChar
    override val defaultKeys: Seq[KeySeq] = Seq("T")
    override def skip(content: Rich, range: IntRange): IntRange = content.moveLeftAtomic(range)
    override def move(content: Rich, range: IntRange, char: Grapheme): Option[IntRange] =
      content.findLeftCharAtomic(range, char.a, delimitationCodePoints).map(r => content.moveRightAtomic(r))
  }



  // LATER they are currently here...
  new MotionCommand {
    override def repeatable: Boolean = true
    override def description: String = "repeat previous find command"
    override def available(a: DocState, commandState: CommandState): Boolean = super.available(a) && commandState.lastFindCommand.isDefined
    override val defaultKeys: Seq[KeySeq] = Seq(";")
    override def action(a: DocState, count: Int, commandState: CommandState): DocTransaction = {
      val lf = commandState.lastFindCommand.get
      lf._1.findGrapheme(a, lf._2, count, skipCurrent = true)
    }
    override protected def action(a: DocState, count: Int): DocTransaction = throw new IllegalStateException("No one should call this if the other implemented")

  }

  new MotionCommand {
    override def repeatable: Boolean = true
    override def description: String = "repeat previous find command's reverse"
    override def available(a: DocState, commandState: CommandState): Boolean = super.available(a) && commandState.lastFindCommand.isDefined
    override val defaultKeys: Seq[KeySeq] = Seq(",")
    override def action(a: DocState, count: Int, commandState: CommandState): DocTransaction = {
      val lf = commandState.lastFindCommand.get
      lf._1.reverse.findGrapheme(a, lf._2, count, skipCurrent = true)
    }
    override protected def action(a: DocState, count: Int): DocTransaction = throw new IllegalStateException("No one should call this if the other implemented")
  }

  /**
    * TODO
    * w     N  w            N words forward
    * W     N  W            N blank-separated WORDs forward
    * e     N  e            forward to the end of the Nth word
    * E     N  E            forward to the end of the Nth blank-separated WORD
    * b     N  b            N words backward
    * B     N  B            N blank-separated WORDs backward
    * ge    N  ge           backward to the end of the Nth word
    * gE    N  gE           backward to the end of the Nth blank-separated WORD
    */
  //        val wordBeginning: Command = ???
  //        val wordEnd: Command = ???
  //        val wordNext: Command = ???
  //        val WordBeginning: Command = ???
  //        val WordEnd: Command = ???
  //        val WordNext: Command = ???



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