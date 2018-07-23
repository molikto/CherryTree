package command

import client.Client
import command.Key._
import doc.{DocState, DocTransaction}
import model.range.IntRange
import monix.reactive.Observable
import monix.reactive.subjects._
import settings.Settings

import scala.util.{Success, Try}

class KeyboardCommandHandler extends Settings with CommandState
{ self : Client =>

  private val richMotion = new defaults.RichMotion()

  private val misc = new defaults.Misc()
  val commands = Seq(
    misc,
    richMotion,
    new defaults.RichInsertEnter(),
    new defaults.RichInsert(),
    new defaults.RichVisual(),
    new defaults.RichChange(),
    new defaults.RichDelete(),
    new defaults.NodeMotion(),
    new defaults.NodeVisual(),
    new defaults.NodeMove(),
    new defaults.NodeDelete(),
    new defaults.NodeFold(),
    new defaults.Scroll()).flatMap(_.commands)

  val commandsByCategory = commands.groupBy(_.category)

  private val status =  BehaviorSubject[CommandStatus](CommandStatus.Empty)
  def commandStatus: Observable[CommandStatus] = status

  private var commandPartConfirmed: KeySeq = Seq.empty
  private var commandsToConfirm = Seq.empty[command.Command]
  private var waitingForCharCommand: (command.Command, KeySeq, String) = null
  private var lastFindCommand_ : (FindCommand, Grapheme) = null

  override def lastFindCommand: Option[(FindCommand, Grapheme)] = Option(lastFindCommand_)

  private var commandCounts: String = ""

  def onBeforeUpdateUpdateCommandState(state: DocState): Unit = {
    if (waitingForCharCommand != null) {
      if (!waitingForCharCommand._1.available(state, this)) {
        clearWaitingForGraphemeCommand()
      }
    }
  }



  // LATER updatable keymap
  private lazy val keyToCommand: Map[Key, Seq[command.Command]] = commands.flatMap(c => c.keys.map(_ -> c)).groupBy(_._1.head).map(a => (a._1, a._2.map(_._2)))

  /**
    * return false if no command is exec and not waiting
    */
  def tryDoCommandExact(): Boolean = {
    val availableCommands = commandsToConfirm.filter(a => a.keys.exists(_.startsWith(commandPartConfirmed)) && a.available(state, this))
    if (availableCommands.isEmpty) {
      val pCounts = commandCounts
      val ptoConfirm = commandPartConfirmed
      commandCounts = ""
      commandPartConfirmed = Seq.empty
      commandsToConfirm = Seq.empty
      if (!commandPartConfirmed.forall(_.isModifier)) {
        status.onNext(CommandStatus.LastNotFound(pCounts, ptoConfirm))
      }
      false
    } else {
      var exacts = availableCommands.filter(_.keys.contains(commandPartConfirmed))
      if (exacts.size > 1) {
        // defferent settings of key might override depending on how the key is set
        val sorted = exacts.map(a => (a ,a.keyLevel(commandPartConfirmed))).sortBy(-_._2)
        if (sorted(1)._2 == sorted.head._2) {
          errors_.update(Some(new Exception("Multiple commands with same key")))
        }
        exacts = Seq(sorted.head._1)
      }
      exacts.headOption match {
        case Some(exact) =>
          val pCounts = commandCounts
          val ptoConfirm = commandPartConfirmed
          commandsToConfirm = Seq.empty
          commandPartConfirmed = Seq.empty
          val c = if (commandCounts.isEmpty) 1 else commandCounts.toInt
          commandCounts = ""
          if (exact.needsChar) {
            waitingForCharCommand = (exact, ptoConfirm, pCounts)
            status.onNext(CommandStatus.WaitingForChar(pCounts, ptoConfirm))
          } else {
            flush()
            val res = exact.action(state, c, this, Some(ptoConfirm))
            change(res)
            val returnFalse = res == DocTransaction.empty && exact.emptyAsFalse
            if (exact == misc.exit) {
              commandCounts = ""
              commandsToConfirm = Seq.empty
              commandPartConfirmed = Seq.empty
              waitingForCharCommand = null
              status.onNext(CommandStatus.Empty)
            } else {
              status.onNext(CommandStatus.LastPerformed(pCounts, ptoConfirm, None))
            }
            if (returnFalse) return false
          }
        case None =>
          commandsToConfirm = availableCommands
          status.onNext(CommandStatus.WaitingForConfirm(commandCounts, commandPartConfirmed))
      }
      true
    }
  }

  def keyDown(key: Key): Boolean = {
    if (waitingForCharCommand != null) {
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
          case _: Key.Modifier if !commandCounts.isEmpty => return true
          case Key.Grapheme(a) if a.isDigit && (commandCounts.length > 0 || a.asDigit != 0) =>
            commandCounts = commandCounts + a
            status.onNext(CommandStatus.InputtingCount(commandCounts))
          case _ =>
            doCommand()
        }
        true
      }
    }
  }

  private def clearWaitingForGraphemeCommand(): Unit = {
    waitingForCharCommand = null
  }

  private def consumeByWaitingForGraphemeCommand(state: DocState, a: Grapheme): DocTransaction = {
    if (waitingForCharCommand != null) {
      val ww = waitingForCharCommand
      ww._1 match {
        case command: FindCommand =>
          lastFindCommand_ = (command, a)
        case _ =>
      }
      val res = ww._1.actionOnGrapheme(state, a, if (ww._3 == "") 1 else ww._3.toInt)
      waitingForCharCommand = null
      status.onNext(CommandStatus.LastPerformed(ww._3, ww._2, Some(a.a)))
      res
    } else {
      DocTransaction.empty
    }
  }

  trait SideEffectingCommand extends Command  {

  }


  new SideEffectingCommand {
    override def category: String = misc.name
    override def description: String = "visit link url"
    override def defaultKeys: Seq[KeySeq] = Seq("gx")

    override def available(a: DocState): Boolean = a.isRichNormalOrVisual && {
      val (_, rich, nv) = a.asRichNormalOrVisual
      val t = rich.info(nv.focus.start).text
      t.isDelimited && t.asDelimited.delimitation.attributes.contains(model.data.UrlAttribute)
    }

    override def action(a: DocState, count: Int): DocTransaction = {
      val (_, rich, nv) = a.asRichNormalOrVisual
      val t = rich.info(nv.focus.start).text
      val url = t.asDelimited.attribute(model.data.UrlAttribute).toString
      import io.lemonlabs.uri._
      Try {Url.parse(url)} match {
        case Success(_) => viewMessages_.onNext(Client.ViewMessage.VisitUrl(url))
        case _ =>
      }
      DocTransaction.empty
    }
  }


}
