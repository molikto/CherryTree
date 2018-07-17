package command

import api.ClientUpdate
import client.Client
import model.data.{apply => _, _}
import model.range.IntRange
import model.{ClientState, mode, operation}

import scala.collection.mutable.ArrayBuffer
import scala.util.Try
import Key._

// comments from quickref.txt  For Vim version 8.1.  Last change: 2018 Apr 18
// also a recent version of Vimflowy
trait Commands {

  /**
    * helpers
    */
  private def noUpdate(): Client.Update = Client.Update(Seq.empty, None)
  private def modeUpdate(a: mode.Node) = Client.Update(Seq.empty, Some(a))

  /**
    * these are settings??
    */
  def delimitationCodePoints: Map[SpecialChar.Delimitation, Unicode]


  def additionalKeyMaps: Map[String, Seq[Key]]
  def removedDefaultKeyMaps: Map[String, Seq[Key]]

  private val commands_ = new ArrayBuffer[Command]()

  private var waitingForCharCommand: Command = null

  // TODO clear in mode change
  def isWaitingForGraphemeCommand: Boolean = waitingForCharCommand != null

  def consumeByWaitingForGraphemeCommand(state: ClientState, a: Grapheme): Client.Update = {
    if (waitingForCharCommand != null) {
      val res = waitingForCharCommand.actionOnGrapheme(state, a)
      waitingForCharCommand = null
      res
    } else {
      noUpdate()
    }
  }

  def clearWaitingForGraphemeCommand(): Unit = {
    waitingForCharCommand = null
  }

  private var lastFindCommand: Command.ContentMotion.FindCommand = null
  private var lastFindCommandArgument: Grapheme = null

  def commands: Seq[Command] = commands_

  abstract class Command {


    commands_.append(this)

    def hardcodeKeys: Seq[Key] = Seq.empty
    def defaultKeys: Seq[Key]

    def keys:  Seq[Key] = defaultKeys ++ hardcodeKeys // TODO key maps

    def repeatable: Boolean = false

    def available(a: ClientState): Boolean

    def action(a: ClientState): Client.Update

    def waitingForGrapheme: Boolean = false
    def actionOnGrapheme(a: ClientState, char: Grapheme): Client.Update = throw new NotImplementedError()
  }


  object Command {

    val exit: Command = new Command {
      override def defaultKeys: Seq[Key] = Seq(Escape, Control + "c", Control + "[")
      override def available(a: ClientState): Boolean = true

      override def action(a: ClientState): Client.Update = {
        a.mode match {
          case Some(m) =>
            m match {
              case model.mode.Node.Visual(fix, move) => noUpdate() // TODO exit visual mode
              case nc@model.mode.Node.Content(n, c) =>
                a.node(n).content match {
                  case model.data.Content.Rich(rich) =>
                    c match {
                      case model.mode.Content.Insert(pos) =>
                        modeUpdate(nc.copy(a = model.mode.Content.Normal(rich.moveLeftAtomic(pos))))
                      case model.mode.Content.Visual(fix, move) =>
                        modeUpdate(nc.copy(a = model.mode.Content.Normal(move)))
                      case _ => noUpdate()
                    }
                  case model.data.Content.Code(c, _) =>
                    ??? // TODO exit code mode?

                }
              }
          case None => noUpdate()
        }
      }
    }
    /**
      */
    object ContentMotion {

      // DIFFERENCE content motion is only available on paragraphs, editing of code is handled by third party editor!!!
      abstract class RichBaseCommand extends Command {

        override def available(a: ClientState): Boolean = a.mode match {
          case Some(mode.Node.Content(n, c)) =>
            a.node(n).content match {
              case model.data.Content.Rich(p) =>
                c match {
                  case mode.Content.Normal(_) => true
                  case mode.Content.Visual(_, _) => true
                  case _ => false
                }
              case _ => false
            }
          case _ => false
        }
      }

      abstract class RichMotionCommand extends RichBaseCommand {

        def move(content: model.data.Rich, a: IntRange): IntRange

        final override def action(a: ClientState): Client.Update = {
          a.mode match {
            case Some(o@mode.Node.Content(n, c)) =>
              val content = a.node(n).content.asInstanceOf[Content.Rich].content
              c match {
                case mode.Content.Normal(r) => modeUpdate(o.copy(a = mode.Content.Normal(move(content, r))))
                case v@mode.Content.Visual(fix, m) => modeUpdate(o.copy(a = mode.Content.Visual(fix, move(content, m))))
                case _ => throw new IllegalArgumentException("Should not call this method with not applicable state")
              }
            case _ => throw new IllegalArgumentException("Should not call this method with not applicable state")
          }
        }
      }


      val left: Command = new RichMotionCommand() { // DIFFERENCE h + Control is also in Vim, but we don't use this,
        override def defaultKeys = Seq("h", Backspace, Left)
        override def move(content: Rich, a: IntRange): IntRange = content.moveLeftAtomic(a)
        override def repeatable: Boolean = true
      }

      val right: Command = new RichMotionCommand() {
        override def defaultKeys = Seq("l", Right)  // DIFFERENCE space is for smart move
        override def move(content: Rich, a: IntRange): IntRange = content.moveRightAtomic(a)
        override def repeatable: Boolean = true
      }

      val beginning: Command = new RichMotionCommand() {
        override def defaultKeys = Seq("0", "^", Home) // DIFFERENCE merged because we are already structural
        override def move(content: Rich, a: IntRange): IntRange = content.beginningAtomicRange()
      }

      val end: Command = new RichMotionCommand {
        override def defaultKeys = Seq("$", End) // DIFFERENCE is not repeatable, different from Vim
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

      // not implemented...??? because it is hard to make columns in a rich text editor
      // bar   N  |            to column N (default: 1)

      abstract class FindCommand extends RichBaseCommand {

        def reverse: FindCommand

        override def action(a: ClientState): Client.Update = {
          waitingForCharCommand = this
          noUpdate()
        }

        override def waitingForGrapheme: Boolean = true
        override def repeatable: Boolean = true
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

        def findGrapheme(a: ClientState, char: Grapheme, skipCurrent: Boolean): Client.Update = {
          a.mode match {
            case Some(o@mode.Node.Content(n, c)) =>
              val content = a.node(n).content.asInstanceOf[Content.Rich].content
              c match {
                case mode.Content.Normal(r) =>
                  moveSkip(content, r, char, skipCurrent) match {
                    case Some(move) =>
                      modeUpdate(o.copy(a = mode.Content.Normal(move)))
                    case None =>
                      noUpdate()
                  }
                case v@mode.Content.Visual(fix, m) =>
                  moveSkip(content, m, char, skipCurrent) match {
                    case Some(move) =>
                      modeUpdate(o.copy(a = mode.Content.Visual(fix, move)))
                    case None =>
                      noUpdate()
                  }
                case _ => throw new IllegalArgumentException("Should not call this method with not applicable state")
              }
            case _ => throw new IllegalArgumentException("Should not call this method with not applicable state")
          }
        }

        final override def actionOnGrapheme(a: ClientState, char: Grapheme): Client.Update = {
          lastFindCommand = this
          lastFindCommandArgument = char
          findGrapheme(a, char, skipCurrent = false)
        }
      }

      val findNextChar: FindCommand = new FindCommand {
        override def reverse: FindCommand = findPreviousChar
        override def defaultKeys: Seq[Key] = Seq("f")
        def move(a: Rich, range: IntRange, char: Grapheme): Option[IntRange] = a.findRightCharAtomic(range, char.a, delimitationCodePoints)

      }
      val findPreviousChar: FindCommand = new FindCommand {
        override def reverse: FindCommand = findNextChar
        override def defaultKeys: Seq[Key] = Seq("F")
        def move(a: Rich, range: IntRange, char: Grapheme): Option[IntRange] = a.findLeftCharAtomic(range, char.a, delimitationCodePoints)
      }
      val toNextChar: FindCommand = new FindCommand {
        override def reverse: FindCommand = toPreviousChar
        override def defaultKeys: Seq[Key] = Seq("t")
        override def skip(content: Rich, range: IntRange): IntRange = content.moveRightAtomic(range)
        override def move(content: Rich, range: IntRange, char: Grapheme): Option[IntRange] =
          content.findRightCharAtomic(range, char.a, delimitationCodePoints).map(r => content.moveLeftAtomic(r))
      }
      val toPreviousChar: FindCommand = new FindCommand {
        override def reverse: FindCommand = toNextChar
        override def defaultKeys: Seq[Key] = Seq("T")
        override def skip(content: Rich, range: IntRange): IntRange = content.moveLeftAtomic(range)
        override def move(content: Rich, range: IntRange, char: Grapheme): Option[IntRange] =
          content.findLeftCharAtomic(range, char.a, delimitationCodePoints).map(r => content.moveRightAtomic(r))
      }
      val repeatFind: Command = new RichBaseCommand {
        override def available(a: ClientState): Boolean = super.available(a) && lastFindCommand != null
        override def defaultKeys: Seq[Key] = Seq(";")
        override def action(a: ClientState): Client.Update = {
          lastFindCommand.findGrapheme(a, lastFindCommandArgument, skipCurrent = true)
        }
        override def repeatable: Boolean = true
      }
      val repeatFindOppositeDirection: Command = new RichBaseCommand {
        override def available(a: ClientState): Boolean = super.available(a) && lastFindCommand != null
        override def defaultKeys: Seq[Key] = Seq(",")
        override def action(a: ClientState): Client.Update = {
          lastFindCommand.reverse.findGrapheme(a, lastFindCommandArgument, skipCurrent = true)
        }
        override def repeatable: Boolean = true
      }

      /**
        * LATER
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


      object NodeMotion {
        /**
          * LATER
          * k     N  k            up N lines (also: CTRL-P and <Up>)
          * j     N  j            down N lines (also: CTRL-J, CTRL-N, <NL>, and <Down>)
          * -     N  -            up N lines, on the first non-blank character
          * +     N  +            down N lines, on the first non-blank character (also:
          * CTRL-M and <CR>)
          * _     N  _            down N-1 lines, on the first non-blank character
          * G     N  G            goto line N (default: last line), on the first
          * non-blank character
          * gg    N  gg           goto line N (default: first line), on the first
          * non-blank character
          */
//        val up: Command = ???
//        val down: Command = ???
//        val visibleBeginning: Command = ???
//        val visibleEnd: Command = ???

        // not implemented for not understand what should it behave
        // * N%    N  %            goto line N percentage down in the file; N must be
        // * given, otherwise it is the % command

        // screen related not implemented
//        gk    N  gk           up N screen lines (differs from "k" when line wraps)
//        gj    N  gj           down N screen lines (differs from "j" when line wraps)


        /**
          * LATER
          * 'motion-parent': [['g', 'p']],
          * 'motion-next-sibling': [['}']],
          * 'motion-prev-sibling': [[' {']],
          */
//        val parent: Command = ???
//        val nextSibling: Command = ???
//        val previousSibling: Command = ???
      }
    }

    object EnterVisual {

    }

    object EnterInsert {
      // DIFFERENCE insert mode doesn't take n currently (Sublime doesn't do this currently, wired)
      //:startinsert  :star[tinsert][!]  start Insert mode, append when [!] used
      //:startreplace :startr[eplace][!]  start Replace mode, at EOL when [!] used
      //
      //in Visual block mode:
      //v_b_I    I    insert the same text in front of all the selected lines
      //v_b_A    A    append the same text after all the selected lines

      // TODO open new line
      //o     N  o    open a new line below the current line, append text (N times)
      //O     N  O    open a new line above the current line, append text (N times)
      abstract class InsertCommand extends Command  {
        def move(content: Rich,a: IntRange): Int

        override def available(a: ClientState): Boolean = a.mode match {
          case Some(model.mode.Node.Content(n, model.mode.Content.Normal(_))) =>
            a.node(n).content match {
              case model.data.Content.Rich(p) =>
                true
              case _ => false
            }
          case _ => false
        }

        override def action(a: ClientState): Client.Update = a.mode match {
          case Some(c@model.mode.Node.Content(n, model.mode.Content.Normal(r))) =>
            modeUpdate(c.copy(a = model.mode.Content.Insert(move(
              a.node(n).content.asInstanceOf[model.data.Content.Rich].content, r))))
        }
      }
      val appendAtCursor: InsertCommand = new InsertCommand {
        override def defaultKeys: Seq[Key] = Seq("a")
        override def move(content: Rich, a: IntRange): Int = a.until
      }
      val appendAtContentEnd: InsertCommand  = new InsertCommand {
        override def defaultKeys: Seq[Key] = Seq("A")
        override def move(content: Rich,a: IntRange): Int = content.size
      }
      val insertAtCursor: InsertCommand  = new InsertCommand {
        override def defaultKeys: Seq[Key] = Seq("i")
        override def move(content: Rich,a: IntRange): Int = a.start
      }
      // TODO support two char keys gI    N  gI   insert text in column 1 (N times)
      val insertAtContentBeginning : InsertCommand = new InsertCommand {
        override def defaultKeys: Seq[Key] = Seq("I")
        override def move(content: Rich,a: IntRange): Int = 0
      }
    }

    object InsertModeEdits {
      abstract class EditCommand extends Command  {
        def defaultKeys: Seq[Key] = Seq.empty
        def edit(content: Rich,a: Int): Option[model.operation.Rich]

        override def available(a: ClientState): Boolean = a.mode match {
          case Some(model.mode.Node.Content(n, model.mode.Content.Insert(_))) =>
            a.node(n).content match {
              case model.data.Content.Rich(p) =>
                true
              case _ => false
            }
          case _ => false
        }

        override def action(a: ClientState): Client.Update = a.mode match {
          case Some(c@model.mode.Node.Content(n, model.mode.Content.Insert(r))) =>
            val res = edit(a.node(n).content.asInstanceOf[model.data.Content.Rich].content, r).map(k => {
              Seq(model.operation.Node.Content(n, model.operation.Content.Rich(k)))
            }).getOrElse(Seq.empty)
            Client.Update(res, None)
        }
      }
      val backspace: Command = new EditCommand {
        // TODO these keys should be seperate delete words, etc...
        override def hardcodeKeys: Seq[Key] = Backspace.withAllModifers ++ (if (model.isMac) Seq(Control + "h") else Seq.empty)
        override def edit(content: Rich, a: Int): Option[operation.Rich] = {
          if (a > 0) {
            Some(operation.Rich.deleteOrUnwrapAt(content, a - 1))
          } else {
            None
          }
        }
      }

      val enter: Command = new EditCommand {
        // TODO what to do on enter???
        override def hardcodeKeys: Seq[Key] = Enter.withAllModifers
        override def edit(content: Rich, a: Int): Option[operation.Rich] = None
      }

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

    object Scroll {

      // LATER only some of should be implemented
      // Q_sc          Scrolling
      //
      //CTRL-E        N  CTRL-E       window N lines downwards (default: 1)
      //CTRL-D        N  CTRL-D       window N lines Downwards (default: 1/2 window)
      //CTRL-F        N  CTRL-F       window N pages Forwards (downwards)
      //CTRL-Y        N  CTRL-Y       window N lines upwards (default: 1)
      //CTRL-U        N  CTRL-U       window N lines Upwards (default: 1/2 window)
      //CTRL-B        N  CTRL-B       window N pages Backwards (upwards)
      //z<CR>            z<CR> or zt  redraw, current line at top of window
      //z.               z.    or zz  redraw, current line at center of window
      //z-               z-    or zb  redraw, current line at bottom of window
      //
      //These only work when 'wrap' is off:
      //zh            N  zh           scroll screen N characters to the right
      //zl            N  zl           scroll screen N characters to the left
      //zH            N  zH           scroll screen half a screenwidth to the right
      //zL            N  zL           scroll screen half a screenwidth to the left
    }
  }

  {
    var ignored: Command = Command.ContentMotion.toNextChar
    ignored = Command.EnterInsert.appendAtCursor
    ignored = Command.EnterVisual.appendAtCursor
    ignored = Command.exit
    ignored = Command.InsertModeEdits.backspace
  }

  // these are currently NOT implemented becuase we want a different mark system
  // also the rest which are useful can be implemented later
  //o
  //        Q_ma          Marks and motions
  //
  //        m        m{a-zA-Z}    mark current position with mark {a-zA-Z}
  //        `a       `{a-z}       go to mark {a-z} within current file
  //          `A       `{A-Z}       go to mark {A-Z} in any file
  //          `0       `{0-9}       go to the position where Vim was previously exited
  //          ``       ``           go to the position before the last jump
  //          `quote   `"           go to the position when last editing this file
  //        `[       `[           go to the start of the previously operated or put text
  //          `]       `]           go to the end of the previously operated or put text
  //          `<       `<           go to the start of the (previous) Visual area
  //        `>       `>           go to the end of the (previous) Visual area
  //        `.       `.           go to the position of the last change in this file
  //        '        '{a-zA-Z0-9[]'"<>.}
  //          same as `, but on the first non-blank in the line
  //            :marks  :marks        print the active marks
  //          CTRL-O  N  CTRL-O     go to Nth older position in jump list
  //          CTRL-I  N  CTRL-I     go to Nth newer position in jump list
  //            :ju     :ju[mps]      print the jump list

  //        %        %            find the next brace, bracket, comment, or "#if"/
  //        "#else"/"#endif" in this line and go to its match
  //        H     N  H            go to the Nth line in the window, on the first
  //        non-blank
  //        M        M            go to the middle line in the window, on the first
  //        non-blank
  //        L     N  L            go to the Nth line from the bottom, on the first
  //        non-blank
  //
  //        go    N  go                   go to Nth byte in the buffer
  //          :go   :[range]go[to] [off]    go to [off] byte in the buffer



  // features not sure about

  // Q_ta          Using tags
  //
  //:ta      :ta[g][!] {tag}      jump to tag {tag}
  //:ta      :[count]ta[g][!]     jump to [count]'th newer tag in tag list
  //CTRL-]      CTRL-]            jump to the tag under cursor, unless changes
  //                                   have been made
  //:ts      :ts[elect][!] [tag]  list matching tags and select one to jump to
  //:tjump   :tj[ump][!] [tag]    jump to tag [tag] or select from list when
  //                                   there are multiple matches
  //:ltag    :lt[ag][!] [tag]     jump to tag [tag] and add matching tags to the
  //                                   location list
  //
  //:tags    :tags                print tag list
  //CTRL-T   N  CTRL-T            jump back from Nth older tag in tag list
  //:po      :[count]po[p][!]     jump back from [count]'th older tag in tag list
  //:tnext   :[count]tn[ext][!]   jump to [count]'th next matching tag
  //:tp      :[count]tp[revious][!] jump to [count]'th previous matching tag
  //:tr      :[count]tr[ewind][!] jump to [count]'th matching tag
  //:tl      :tl[ast][!]          jump to last matching tag
  //
  //:ptag    :pt[ag] {tag}        open a preview window to show tag {tag}
  //CTRL-W_}    CTRL-W }          like CTRL-] but show tag in preview window
  //:pts     :pts[elect]          like ":tselect" but show tag in preview window
  //:ptjump  :ptj[ump]            like ":tjump" but show tag in preview window
  //:pclose  :pc[lose]            close tag preview window
  //CTRL-W_z    CTRL-W z          close tag preview window


  //Q_di          Digraphs
  //
  //:dig     :dig[raphs]          show current list of digraphs
  //:dig     :dig[raphs] {char1}{char2} {number} ...
  //                                add digraph(s) to the list
  //
  //In Insert or Command-line mode:
  //i_CTRL-K      CTRL-K {char1} {char2}
  //                                  enter digraph
  //i_digraph     {char1} <BS> {char2}
  //                                  enter digraph if 'digraph' option set
  //------------------------------------------------------------------------------
  //Q_si          Special inserts
  //
  //:r       :r [file]       insert the contents of [file] below the cursor
  //:r!      :r! {command}   insert the standard output of {command} below the
  //                              cursor

}
