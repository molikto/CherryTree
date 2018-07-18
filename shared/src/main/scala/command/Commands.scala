package command

import api.ClientUpdate
import client.Client
import model.data.{apply => _, _}
import model.range.IntRange
import model.{ClientState, cursor, mode, operation}

import scala.collection.mutable.ArrayBuffer
import scala.util.Try
import Key._
import monix.reactive.subjects.PublishSubject
import util.ObservableProperty

// comments from quickref.txt  For Vim version 8.1.  Last change: 2018 Apr 18
// also a recent version of Vimflowy

abstract class Command {

  def hardcodeKeys: Seq[KeySeq] = Seq.empty
  def defaultKeys: Seq[KeySeq]

  def keys:  Seq[KeySeq] = defaultKeys ++ hardcodeKeys // TODO key maps

  def available(a: ClientState): Boolean

  def action(a: ClientState, count: Int): Client.Update

  def waitingForGrapheme: Boolean = false
  def actionOnGrapheme(a: ClientState, char: Grapheme, count: Int): Client.Update = throw new NotImplementedError()
}

trait Commands { self: Client =>


  private var commandPartConfirmed: KeySeq = Seq.empty
  private var commandsToConfirm = Seq.empty[Command]
  private var waitingForCharCommand: (Command, Int) = null

  private var commandCounts: String = ""
  def commandCountsText: String = commandCounts
  def commandNotConfirmed: KeySeq = commandPartConfirmed

  def onBeforeUpdateUpdateCommandState(state: ClientState): Unit = {
    if (waitingForCharCommand != null) {
      if (!waitingForCharCommand._1.available(state)) {
        clearWaitingForGraphemeCommand()
      }
    }
  }

  private val commands_ = new ArrayBuffer[Command]()

  {
    var ignored: Command = Command.RichMotion.toNextChar
    ignored = Command.EnterRichInsert.appendAtCursor
    ignored = Command.NodeMotion.up
    ignored = Command.OpenAndEnterRichInsert.openAbove
    ignored = Command.RichVisual.enter
    ignored = Command.NodeVisual.enter
    ignored = Command.exit
    ignored = Command.Delete.deleteAfterVisual
    ignored = Command.RichInsertModeEdits.backspace
  }
  // LATER updatable keymap
  private val keyToCommand: Map[Key, Seq[Command]] = commands.flatMap(c => c.keys.map(_ -> c)).groupBy(_._1.head).map(a => (a._1, a._2.map(_._2)))

  /**
    * return false if no command is exec and not waiting
    */
  def tryDoCommandExact(): Boolean = {
    val availableCommands = commandsToConfirm.filter(a => a.keys.exists(_.startsWith(commandPartConfirmed)) && a.available(state))
    if (availableCommands.isEmpty) {
      commandPartConfirmed = Seq.empty
      commandsToConfirm = Seq.empty
      false
    } else {
      val exacts = availableCommands.filter(_.keys.contains(commandPartConfirmed))
      if (exacts.size > 1) errors.update(Some(new Exception("Multiple commands with same key")))
      exacts.headOption match {
        case Some(exact) =>
          commandsToConfirm = Seq.empty
          commandPartConfirmed = Seq.empty
          val c = if (commandCounts.isEmpty) 1 else commandCounts.toInt
          commandCounts = ""
          act(exact, c)
        case None =>
          commandsToConfirm = availableCommands
      }
      true
    }
  }

  def keyDown(key: Key): Boolean = {
    if (isWaitingForGraphemeCommand) {
      key.a match {
        case g@Key.Grapheme(a) => change(consumeByWaitingForGraphemeCommand(state, g))
        case _: Key.Modifier => // ignore modifier only keys
        case _ => clearWaitingForGraphemeCommand()
      }
      true
    } else if (commandPartConfirmed.nonEmpty) {
      key.a match {
        case _: Key.Modifier => true // ignore modifier only keys
        case _ =>
          commandPartConfirmed = commandPartConfirmed :+ key
          tryDoCommandExact()
          true// always handled
      }
    } else {
      def doCommand(): Boolean = {
        commandsToConfirm = keyToCommand.getOrElse(key, Seq.empty)
        commandPartConfirmed = Seq(key)
        tryDoCommandExact()
      }
      if (state.isRichInserting) {
        // some keys we MUST keep
        doCommand()
      } else {
        key.a match {
          case Key.Grapheme(a) if a.isDigit && (commandCounts.length > 0 || a.asDigit != 0) => commandCounts = commandCounts + a
          case _ =>
            doCommand()
        }
        true
      }
    }
  }
  /**
    * helpers
    */
  private def noUpdate(): Client.Update = Client.Update(Seq.empty, None)
  private def modeUpdate(a: mode.Node) = Client.Update(Seq.empty, Some(a))

  /**
    * these are settings??
    */
  def delimitationCodePoints: Map[SpecialChar.Delimitation, Unicode]


  def additionalKeyMaps: Map[String, Seq[KeySeq]]
  def removedDefaultKeyMaps: Map[String, Seq[KeySeq]]

  def isWaitingForGraphemeCommand: Boolean = waitingForCharCommand != null


  def consumeByWaitingForGraphemeCommand(state: ClientState, a: Grapheme): Client.Update = {
    if (waitingForCharCommand != null) {
      val res = waitingForCharCommand._1.actionOnGrapheme(state, a, waitingForCharCommand._2)
      waitingForCharCommand = null
      res
    } else {
      noUpdate()
    }
  }

  def clearWaitingForGraphemeCommand(): Unit = {
    waitingForCharCommand = null
  }

  private var lastFindCommand: Command.RichMotion.FindCommand = null
  private var lastFindCommandArgument: Grapheme = null

  def commands: Seq[Command] = commands_


  abstract class Command extends command.Command {
    commands_.append(this)
  }

  object Command {

    val exit: Command = new Command {
      override val defaultKeys: Seq[KeySeq] = Seq(Escape, Control + "c", Control + "[")
      override def available(a: ClientState): Boolean = true

      override def action(a: ClientState, ignore: Int): Client.Update = {
        // LATER is this good? modify state on the go???
        commandCounts = ""
        commandsToConfirm = Seq.empty
        commandPartConfirmed = Seq.empty
        waitingForCharCommand = null
        a.mode match {
          case Some(m) =>
            m match {
              case model.mode.Node.Visual(fix, move) =>
                modeUpdate(model.data.Node.defaultNormalMode(a.node, move))
              case nc@model.mode.Node.Content(n, c) =>
                a.node(n).content match {
                  case model.data.Content.Rich(rich) =>
                    c match {
                      case model.mode.Content.RichInsert(pos) =>
                        val range = if (pos != 0) rich.moveLeftAtomic(pos)
                        else if (rich.isEmpty) IntRange(0, 0)
                        else rich.beginningAtomicRange()
                        modeUpdate(a.copyContentMode(model.mode.Content.RichNormal(range)))
                      case model.mode.Content.RichVisual(_, move) =>
                        modeUpdate(a.copyContentMode(model.mode.Content.RichNormal(move)))
                      case _ => noUpdate()
                    }
                  case model.data.Content.Code(_, _) =>
                    c match {
                      case model.mode.Content.CodeInside =>
                        modeUpdate(a.copyContentMode(model.mode.Content.CodeNormal))
                      case _ => noUpdate()
                    }
                  case _ =>
                    noUpdate()
                }
              }
          case None => noUpdate()
        }
      }
    }

    abstract class MotionCommand extends Command {
      override def available(a: ClientState): Boolean = a.isRichNormalOrVisual
    }
    /**
      */
    object RichMotion {

      // DIFFERENCE content motion is only available on paragraphs, editing of code is handled by third party editor!!!

      abstract class RichMotionCommand extends MotionCommand {

        def move(content: model.data.Rich, a: IntRange): IntRange

        final override def action(a: ClientState, count: Int): Client.Update = {
          val (_, content, m) = a.asRichNormalOrVisual
          def act(r: IntRange) = (0 until count).foldLeft(r) { (rr, _) => move(content, rr) }
          m match {
            case mode.Content.RichNormal(r) => modeUpdate(a.copyContentMode(mode.Content.RichNormal(act(r))))
            case mode.Content.RichVisual(fix, k) => modeUpdate(a.copyContentMode(mode.Content.RichVisual(fix, act(k))))
          }
        }
      }


      val left: Command = new RichMotionCommand() { // DIFFERENCE h + Control is also in Vim, but we don't use this,
        override val defaultKeys = Seq("h", Backspace, Left)
        override def move(content: Rich, a: IntRange): IntRange = content.moveLeftAtomic(a)
      }

      val right: Command = new RichMotionCommand() {
        override val defaultKeys = Seq("l", Right)  // DIFFERENCE space is for smart move
        override def move(content: Rich, a: IntRange): IntRange = content.moveRightAtomic(a)
      }

      val beginning: Command = new RichMotionCommand() {
        override val defaultKeys = Seq("0", "^", Home) // DIFFERENCE merged because we are already structural
        override def move(content: Rich, a: IntRange): IntRange = content.beginningAtomicRange()
      }

      val end: Command = new RichMotionCommand {
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

      // not implemented...??? because it is hard to make columns in a rich text editor
      // bar   N  |            to column N (default: 1)

      abstract class FindCommand extends MotionCommand {

        def reverse: FindCommand

        override def action(a: ClientState, count: Int): Client.Update = {
          waitingForCharCommand = (this, count)
          noUpdate()
        }

        override def waitingForGrapheme: Boolean = true
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

        def findGrapheme(a: ClientState, char: Grapheme, count: Int, skipCurrent: Boolean): Client.Update = {
          val (_, content, mm) = a.asRichNormalOrVisual
          def act(r: IntRange) = (0 until count).foldLeft(Some(r): Option[IntRange]) {(r, i) =>
            r.flatMap(rr => moveSkip(content, rr, char, skipCurrent || i > 0))
          }
          mm match {
            case mode.Content.RichNormal(r) =>
              act(r) match {
                case Some(move) =>
                  modeUpdate(a.copyContentMode(mode.Content.RichNormal(move)))
                case None =>
                  noUpdate()
              }
            case mode.Content.RichVisual(fix, m) =>
              act(m) match {
                case Some(move) =>
                  modeUpdate(a.copyContentMode( mode.Content.RichVisual(fix, move)))
                case None =>
                  noUpdate()
              }
          }
        }

        final override def actionOnGrapheme(a: ClientState, char: Grapheme, count: Int): Client.Update = {
          lastFindCommand = this
          lastFindCommandArgument = char
          findGrapheme(a, char, count, skipCurrent = false)
        }
      }

      val findNextChar: FindCommand = new FindCommand {
        override def reverse: FindCommand = findPreviousChar
        override val defaultKeys: Seq[KeySeq] = Seq("f")
        def move(a: Rich, range: IntRange, char: Grapheme): Option[IntRange] = a.findRightCharAtomic(range, char.a, delimitationCodePoints)

      }
      val findPreviousChar: FindCommand = new FindCommand {
        override def reverse: FindCommand = findNextChar
        override val defaultKeys: Seq[KeySeq] = Seq("F")
        def move(a: Rich, range: IntRange, char: Grapheme): Option[IntRange] = a.findLeftCharAtomic(range, char.a, delimitationCodePoints)
      }
      val toNextChar: FindCommand = new FindCommand {
        override def reverse: FindCommand = toPreviousChar
        override val defaultKeys: Seq[KeySeq] = Seq("t")
        override def skip(content: Rich, range: IntRange): IntRange = content.moveRightAtomic(range)
        override def move(content: Rich, range: IntRange, char: Grapheme): Option[IntRange] =
          content.findRightCharAtomic(range, char.a, delimitationCodePoints).map(r => content.moveLeftAtomic(r))
      }
      val toPreviousChar: FindCommand = new FindCommand {
        override def reverse: FindCommand = toNextChar
        override val defaultKeys: Seq[KeySeq] = Seq("T")
        override def skip(content: Rich, range: IntRange): IntRange = content.moveLeftAtomic(range)
        override def move(content: Rich, range: IntRange, char: Grapheme): Option[IntRange] =
          content.findLeftCharAtomic(range, char.a, delimitationCodePoints).map(r => content.moveRightAtomic(r))
      }
      val repeatFind: Command = new MotionCommand {
        override def available(a: ClientState): Boolean = super.available(a) && lastFindCommand != null
        override val defaultKeys: Seq[KeySeq] = Seq(";")
        override def action(a: ClientState, count: Int): Client.Update = {
          lastFindCommand.findGrapheme(a, lastFindCommandArgument, count, skipCurrent = true)
        }
      }
      val repeatFindOppositeDirection: Command = new MotionCommand {
        override def available(a: ClientState): Boolean = super.available(a) && lastFindCommand != null
        override val defaultKeys: Seq[KeySeq] = Seq(",")
        override def action(a: ClientState, count: Int): Client.Update = {
          lastFindCommand.reverse.findGrapheme(a, lastFindCommandArgument, count, skipCurrent = true)
        }
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


    }

    object NodeMotion {
      /**
        * LATER
        * CTRL-M and <CR>)
        * _     N  _            down N-1 lines, on the first non-blank character
        */
      abstract class NodeMotionCommand extends Command {
        def move(data: ClientState, a: cursor.Node): Option[cursor.Node]


        override def available(a: ClientState): Boolean = a.mode match {
          case Some(m) => m match {
            case model.mode.Node.Visual(fix, move) => true
            case model.mode.Node.Content(n, cc) => cc match {
              case model.mode.Content.RichNormal(n) => true
              case model.mode.Content.CodeNormal => true
              case _ => false
            }
          }
          case None => false
        }

        final override def action(a: ClientState, count: Int): Client.Update = {
          def act(r: cursor.Node): cursor.Node = (0 until count).foldLeft(r) {(r, _) => move(a, r).getOrElse(r)}
          Client.Update(Seq.empty, Some(a.mode match {
            case Some(m) => m match {
              case v@model.mode.Node.Visual(_, mm) => v.copy(move = act(mm))
              case kkk@model.mode.Node.Content(n, cc) => cc match {
                case _: model.mode.Content.Normal =>
                  model.data.Node.defaultNormalMode(a.node, act(n))
                case _ => throw new MatchError("Not allowed")
              }
            }
            case None => throw new MatchError("Not allowed")
          }))
        }
      }
      val up: Command = new NodeMotionCommand {
         // DIFFERENCE we always go to first char now
        // DIFFERENCE k and - is merged
        override val defaultKeys: Seq[KeySeq] = Seq("k", "-")
        override def move(data: ClientState, a: cursor.Node): Option[cursor.Node] = data.mover().visualUp(a)
      }
      val down: Command = new NodeMotionCommand {
        override val defaultKeys: Seq[KeySeq] = Seq("j", "+")
        override def move(data: ClientState, a: cursor.Node): Option[cursor.Node] = data.mover().visualDown(a)
      }
      val parent: Command = new NodeMotionCommand {
        override val defaultKeys: Seq[KeySeq] = Seq("gp")
        override def move(data: ClientState, a: cursor.Node): Option[cursor.Node] = data.mover().parent(a)
      }
      val nextSibling: Command = new NodeMotionCommand {
        override val defaultKeys: Seq[KeySeq] = Seq("}")
        override def move(data: ClientState, a: cursor.Node): Option[cursor.Node] = data.mover().next(a)
      }
      val previousSibling: Command = new NodeMotionCommand {
        override val defaultKeys: Seq[KeySeq] = Seq("{")
        override def move(data: ClientState, a: cursor.Node): Option[cursor.Node] = data.mover().previous(a)
      }
      val visibleBeginning: Command = new NodeMotionCommand {
        override val defaultKeys: Seq[KeySeq] = Seq("gg")
        override def move(data: ClientState, a: cursor.Node): Option[cursor.Node] = Some(cursor.Node.root)
      }
      val visibleEnd: Command = new NodeMotionCommand {
        override val defaultKeys: Seq[KeySeq] = Seq("G")
        override def move(data: ClientState, a: cursor.Node): Option[cursor.Node] = Some(data.mover().visualBottom(cursor.Node.root))
      }

      // not implemented for not understand what should it behave
      // * N%    N  %            goto line N percentage down in the file; N must be
      // * given, otherwise it is the % command

      // screen related not implemented
      //        gk    N  gk           up N screen lines (differs from "k" when line wraps)
      //        gj    N  gj           down N screen lines (differs from "j" when line wraps)


    }

    object RichVisual {
      val enter: Command = new Command {
        override val defaultKeys: Seq[KeySeq] = Seq("v")
        override def available(a: ClientState): Boolean = a.isRichNormalOrVisual
        override def action(a: ClientState, count: Int): Client.Update = {
          val (_, rich, m) = a.asRichNormalOrVisual
          m match {
            case model.mode.Content.RichNormal(r) =>
              if (rich.isEmpty) {
                noUpdate()
              } else {
                modeUpdate(a.copyContentMode(model.mode.Content.RichVisual(r, r)))
              }
            case model.mode.Content.RichVisual(fix, move) =>
              modeUpdate(a.copyContentMode(model.mode.Content.RichNormal(move)))
          }
        }
      }

      val swap: Command = new Command {
        override val defaultKeys: Seq[KeySeq] = Seq("o")
        override def available(a: ClientState): Boolean = a.isRichVisual
        override def action(a: ClientState, count: Int): Client.Update = modeUpdate(a.copyContentMode(a.asRichVisual._3.swap))
      }
    }

    object NodeVisual {
      // DIFFERENCE going from node visual to content visual is NOT possible
      // CTRL-V   CTRL-V       start highlighting blockwise   }  highlighted text
      // v_CTRL-V CTRL-V       highlight blockwise or stop highlighting
      // gv       gv           start highlighting on previous visual area

    val enter: Command = new Command {
        override val defaultKeys: Seq[KeySeq] = Seq("V") // DIFFERENCE merged two command
        override def available(a: ClientState): Boolean = a.mode match {
          case Some(m) => m match {
            case model.mode.Node.Content(_, mm) => mm.isNormalOrVisual
            case model.mode.Node.Visual(_, _) => true
            case _ => false
          }
          case None => false
        }
        override def action(a: ClientState, count: Int): Client.Update = a.mode match {
          case Some(m) => m match {
            case model.mode.Node.Content(at, mm) if mm.isNormalOrVisual =>
              modeUpdate(mode.Node.Visual(at, at))
            case model.mode.Node.Visual(_, move) =>
              modeUpdate(model.data.Node.defaultNormalMode(a.node, move))
            case _ => throw new IllegalArgumentException("Wrong branch")
          }
          case None => throw new IllegalArgumentException("Wrong branch")
        }
      }
      val swap: Command = new Command {
        override val defaultKeys: Seq[KeySeq] = Seq("o")
        override def available(a: ClientState): Boolean = a.isNodeVisual
        override def action(a: ClientState, count: Int): Client.Update = modeUpdate(a.asNodeVisual.swap)
      }
    }


    object OpenAndEnterRichInsert {

      // DIFFERENCE: currently not repeatable
      val openBellow: Command = new Command {
        override val defaultKeys: Seq[KeySeq] = Seq("o")
        override def available(a: ClientState): Boolean = a.isNormal
        override def action(a: ClientState, count: Int): Client.Update = {
          val pos = a.asNormal._1
          val mover = a.mover()
          val insertionPoint = if (pos == cursor.Node.root) {
            Seq(0)
          } else {
            mover.firstChild(pos).getOrElse(mover.nextOver(pos))
          }
          Client.Update(
            Seq(operation.Node.Insert(insertionPoint, Seq(model.data.Node.empty))),
            Some(model.mode.Node.Content(insertionPoint, model.mode.Content.RichInsert(0))))
        }
      }

      val openAbove: Command = new Command {
        override val defaultKeys: Seq[KeySeq] = Seq("O")
        override def available(a: ClientState): Boolean = a.isNormal
        override def action(a: ClientState, count: Int): Client.Update = {
          val pos = a.asNormal._1
          if (pos == cursor.Node.root) {
            // LATER wrap?
            noUpdate()
          } else {
            Client.Update(
              Seq(operation.Node.Insert(pos, Seq(model.data.Node.empty))),
              Some(model.mode.Node.Content(pos, model.mode.Content.RichInsert(0))))
          }
        }
      }
    }

    object EnterRichInsert {
      // DIFFERENCE insert mode doesn't take n currently (Sublime doesn't do this currently, wired)
      //:startinsert  :star[tinsert][!]  start Insert mode, append when [!] used
      //:startreplace :startr[eplace][!]  start Replace mode, at EOL when [!] used
      //
      //in Visual block mode:
      //v_b_I    I    insert the same text in front of all the selected lines
      //v_b_A    A    append the same text after all the selected lines

      abstract class EnterInsertCommand extends Command {
        def move(content: Rich,a: IntRange): Int

        override def available(a: ClientState): Boolean = a.isRichNormal

        override def action(a: ClientState, count: Int): Client.Update =  {
          val (content, normal) = a.asRichNormal
          modeUpdate(a.copyContentMode(model.mode.Content.RichInsert(move(
            content, normal.range))))
        }
      }
      val appendAtCursor: EnterInsertCommand = new EnterInsertCommand {
        override val defaultKeys: Seq[KeySeq] = Seq("a")
        override def move(content: Rich, a: IntRange): Int = a.until
      }
      val appendAtContentEnd: EnterInsertCommand  = new EnterInsertCommand {
        override val defaultKeys: Seq[KeySeq] = Seq("A")
        override def move(content: Rich,a: IntRange): Int = content.size
      }
      val insertAtCursor: EnterInsertCommand  = new EnterInsertCommand {
        override val defaultKeys: Seq[KeySeq] = Seq("i")
        override def move(content: Rich,a: IntRange): Int = a.start
      }
      // TODO support two char keys gI    N  gI   insert text in column 1 (N times)
      val insertAtContentBeginning : EnterInsertCommand = new EnterInsertCommand {
        override val defaultKeys: Seq[KeySeq] = Seq("I")
        override def move(content: Rich,a: IntRange): Int = 0
      }
    }

    object RichInsertModeEdits {
      abstract class EditCommand extends Command  {
        def defaultKeys: Seq[KeySeq] = Seq.empty
        def edit(content: Rich,a: Int): Option[model.operation.Rich]

        override def available(a: ClientState): Boolean = a.mode match {
          case Some(model.mode.Node.Content(n, model.mode.Content.RichInsert(_))) =>
            a.node(n).content match {
              case model.data.Content.Rich(p) =>
                true
              case _ => false
            }
          case _ => false
        }

        override def action(a: ClientState, count: Int): Client.Update = {
          val (n, content, insert) = a.asRichInsert
          val res = edit(content, insert.pos).map(k => {
            Seq(model.operation.Node.Content(n, model.operation.Content.Rich(k)))
          }).getOrElse(Seq.empty)
          Client.Update(res, None)
        }
      }
      val backspace: Command = new EditCommand {
        // TODO these keys should be seperate delete words, etc...
        override def hardcodeKeys: Seq[KeySeq] = Backspace.withAllModifers ++ (if (model.isMac) Seq(Control + "h") else Seq.empty[KeySeq])
        override def edit(content: Rich, a: Int): Option[operation.Rich] = {
          if (a > 0) {
            Some(operation.Rich.deleteOrUnwrapAt(content, a - 1)) // we don't explicitly set mode, as insert mode transformation is always correct
          } else {
            None
          }
        }
      }

      val enter: Command = new EditCommand {
        // TODO what to do on enter???
        override def hardcodeKeys: Seq[KeySeq] = Enter.withAllModifers
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

    object Delete {
      // LATER
      // J     N  J            join N-1 lines (delete <EOL>s)
      //v_J      {visual}J    join the highlighted lines
      //gJ    N  gJ           like "J", but without inserting spaces
      //v_gJ     {visual}gJ   like "{visual}J", but without inserting spaces
      //:d    :[range]d [x]   delete [range] lines [into register x]

      private def deleteRichNormalRange(a: ClientState, pos: cursor.Node, r: IntRange): Client.Update = {
        val soc = new ArrayBuffer[IntRange]()
        val roc = new ArrayBuffer[IntRange]()
        val rich = a.node(pos).content.asInstanceOf[Content.Rich].content
        val ifs = rich.info(r)
        for (i <- ifs) {
          if (i.isStart) {
            val contentSize = i.text.asInstanceOf[Text.Delimited[Any]].contentSize
            val end = i.nodeStart + i.text.size - 1
            if (!r.contains(end)) {
              soc.append(IntRange(i.nodeStart))
              roc.append(IntRange(i.nodeStart + contentSize + 1, i.nodeStart + i.text.size))
            }
          } else if (i.isEnd) {
            val start = i.nodeStart
            if (!r.contains(start)) {
              val contentSize = i.text.asInstanceOf[Text.Delimited[Any]].contentSize
              soc.append(IntRange(i.nodeStart + contentSize + 1, i.nodeStart + i.text.size))
              roc.append(IntRange(i.nodeStart))
            }
          }
        }
        val ds = (Seq(r.start) ++ soc.flatMap(a => Seq(a.start, a.until)) ++ Seq(r.until)).grouped(2).map(seq => IntRange(seq.head, seq(1))).filter(_.nonEmpty).toSeq
        def deleteRanges(i: Seq[IntRange]): Client.Update = {
          val posTo = rich.moveRightAtomic(r).moveByOrZeroZero(-i.filter(_.until <= r.until).map(_.size).sum)
          Client.Update(
            Seq(operation.Node.Content(pos,
              operation.Content.Rich(operation.Rich.delete(i))
            )),
            Some(mode.Node.Content(pos, mode.Content.RichNormal(posTo)))
          )
        }
        if (ds.isEmpty) {
          if (ifs.forall(_.isStart)) {
            deleteRanges((roc ++ Seq(r)).sortBy(-_.start))
          } else if (ifs.forall(_.isEndOrAttributeTagOrContent)) {
            deleteRanges((roc ++ Seq(r)).sortBy(-_.start))
          } else {
            noUpdate()
          }
        } else {
          deleteRanges(ds.reverse)
        }
      }

      private def deleteNodeRange(a: ClientState, rr: model.range.Node): Client.Update = {
        val parent = a.node(rr.parent)
        val r = rr.copy(childs = IntRange(rr.childs.start, rr.childs.until min parent.childs.size))
        Client.Update(Seq(operation.Node.Delete(r)), {
          val (nowPos, toPos) = if (a.node.get(r.until).isDefined) {
            (r.until, r.start)
          } else if (r.childs.start > 0) {
            val p = r.parent :+ (r.childs.start - 1)
            (p, p)
          } else {
            (r.parent, r.parent)
          }
          Some(model.mode.Node.Content(toPos, a.node(nowPos).content.defaultNormalMode()))
        })
      }

      val deleteAfterVisual: Command = new Command {
        override val defaultKeys: Seq[KeySeq] = Seq("d", "D", "x", "X", Key.Delete)
        override def available(a: ClientState): Boolean = a.isVisual
        override def action(a: ClientState, count: Int): Client.Update = a.mode match {
          case Some(model.mode.Node.Visual(fix , move)) =>
            cursor.Node.minimalRange(fix, move).map(r => deleteNodeRange(a, r)).getOrElse(noUpdate())
          case Some(model.mode.Node.Content(pos, model.mode.Content.RichVisual(f, m))) =>
            deleteRichNormalRange(a, pos, f.merge(m))
          case _ => throw new IllegalArgumentException("Invalid command")
        }
      }

      val delete: Command = new Command {
        override val defaultKeys: Seq[KeySeq] = Seq("x", Key.Delete)
        override def available(a: ClientState): Boolean = a.isNormal
        override def action(a: ClientState, count: Int): Client.Update = {
          val (pos, normal) = a.asNormal
          normal match {
            case model.mode.Content.RichNormal(r) =>
              val rich = a.node(pos).content.asInstanceOf[model.data.Content.Rich].content
              val fr = (1 until count).foldLeft(r) {(r, _) => rich.moveRightAtomic(r) }
              deleteRichNormalRange(a, pos, r.merge(fr))
            case model.mode.Content.CodeNormal =>
              Client.Update(Seq(operation.Node.Replace(pos, a.node(pos).content.asInstanceOf[model.data.Content.Code].copy(unicode = Unicode.empty))),
                a.mode)
          }
        }
      }

      val deleteBefore: Command = new Command {
        override val defaultKeys: Seq[KeySeq] = Seq("X")
        override def available(a: ClientState): Boolean = a.isNormal
        override def action(a: ClientState, count: Int): Client.Update = {
          val (pos, normal) = a.asNormal
          normal match {
            case model.mode.Content.RichNormal(r) =>
              val rich = a.node(pos).content.asInstanceOf[model.data.Content.Rich].content
              val rr = rich.moveLeftAtomic(r)
              val fr = (1 until count).foldLeft(rr) {(r, _) => rich.moveLeftAtomic(r) }
              deleteRichNormalRange(a, pos, rr.merge(fr))
            case model.mode.Content.CodeNormal =>
              Client.Update(Seq(operation.Node.Replace(pos, a.node(pos).content.asInstanceOf[model.data.Content.Code].copy(unicode = Unicode.empty))),
                a.mode)
          }
        }
      }

//      // TODO dw etc
//      val deleteMotion: Command = new Command {
//        override val defaultKeys: Seq[KeySeq] = Seq("d")
//        override def available(a: ClientState): Boolean = a.isNormal
//
//        override def action(a: ClientState, count: Int): Client.Update = {
//          noUpdate()
//        }
//      }

      val deleteLines: Command = new Command {
        override val defaultKeys: Seq[KeySeq] = Seq("dd")
        override def available(a: ClientState): Boolean = a.isNormal
        override def action(a: ClientState, count: Int): Client.Update = {
          val r = a.asNormal._1
          if (r == cursor.Node.root) noUpdate()
          else deleteNodeRange(a, model.range.Node(a.asNormal._1, count))
        }
      }
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
