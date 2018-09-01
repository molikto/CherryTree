package command

import client.Client
import command.Key._
import command.Part.IdentifiedCommand
import command.defaults.YankPaste
import doc.{DocState, DocTransaction}
import model.data.{SpecialChar, Unicode}
import model.range.IntRange
import monix.execution.Cancelable
import monix.reactive.Observable
import monix.reactive.subjects._
import register.{RegisterHandler, Registerable}
import settings.Settings

import concurrent.duration._
import monix.execution.Scheduler.Implicits.global

import scala.collection.mutable.ArrayBuffer
import scala.util.{Success, Try}

abstract class CommandHandler extends Settings with CommandInterface {
  self: Client =>


  val miscCommands = new defaults.Misc(this)
  val yankPaste = new defaults.YankPaste()


  private val defaultCategories =  Seq(
    miscCommands,
    new defaults.RichMotion(),
    new defaults.RichTextObject(),
    new defaults.RichInsertEnter(),
    new defaults.RichInsert(),
    new defaults.RichVisual(),
    new defaults.RichChange(),
    new defaults.RichSpecial(),
    new defaults.RichDelete(),
    new defaults.NodeMotion(),
    new defaults.NodeVisual(),
    new defaults.NodeMove(),
    new defaults.NodeDelete(),
    new defaults.NodeFold(),
    new defaults.NodeMisc(),
    yankPaste,
    new defaults.UndoRedo(),
    new defaults.Scroll(),
  )
  val commands: Seq[Command] = defaultCategories.flatMap(_.commands)


  private val insertModeCommands = commands.filter(_.emptyAsFalseInInsertMode)

  val commandsByCategory: Seq[(String, Seq[Command])] = defaultCategories.map(a => (a.name, a.commands))

  //  private val status =  BehaviorSubject[CommandStatus](CommandStatus.Empty)
  //  def commandStatus: Observable[CommandStatus] = status

  private val buffer = new ArrayBuffer[Part]()

  private val commandBufferUpdates_ = BehaviorSubject[Seq[Part]](buffer)

  def commandBufferUpdates: Observable[Seq[Part]] = commandBufferUpdates_

  private var lastFindCommand_ : (FindCommand, Unicode) = null

  override def lastFindCommand: Option[(FindCommand, Unicode)] = Option(lastFindCommand_)

  override def commandBuffer: Seq[Part] = buffer

  def onBeforeUpdateUpdateCommandState(state: DocState): Unit = {
    var finished = false
    var av = true
    for (i <- buffer.indices) {
      buffer(i) match {
        case IdentifiedCommand(_, c, _) =>
          if (!c.available(state, new CommandInterfaceAvailable {
            override def lastFindCommand: Option[(FindCommand, Unicode)] = CommandHandler.this.lastFindCommand
            override def commandBuffer: Seq[Part] = buffer.take(i - 1)
          })) {
            av = false
          }
        case _: Part.Finished =>
          finished = true
        case _ =>
      }
    }
    if (!finished && !av) {
      buffer.clear()
      commandBufferUpdates_.onNext(buffer)
    }
  }

  // LATER updatable keymap
  private lazy val keyToCommand: Map[Key, Seq[command.Command]] = commands.flatMap(c => c.keys.map(_ -> c)).groupBy(_._1.head).map(a => (a._1, a._2.map(_._2)))

  def parseCommand(key: Key, mergeForStrong: Boolean = true): Unit = {
    var removeForStrong: Part = null
    val (ks, cs0) = buffer.lastOption match {
      case Some(Part.UnidentifiedCommand(kks, ccs)) =>
        buffer.remove(buffer.size - 1)
        (kks, ccs)
      case Some(Part.IdentifiedCommand(Some(kks), css, others)) if mergeForStrong && !css.needsStuff && others.nonEmpty =>
        removeForStrong = buffer.remove(buffer.size - 1)
        (kks, others)
      case _ => (Seq.empty, keyToCommand.getOrElse(key, Seq.empty))
    }
    val cs = if (isInsertOverride) {
      cs0.filter(_.emptyAsFalseInInsertMode)
    } else {
      cs0
    }
    val kk = ks :+ key
    val ac = cs.filter(a => a.keys.exists(_.startsWith(kk)) && a.available(state, this))
    if (ac.isEmpty) {
      if (removeForStrong != null) {
        buffer.append(removeForStrong)
        tryComplete(false)
        clearPreviousCommand()
        parseCommand(key, false)
      } else {
        buffer.append(Part.UnknownCommand(kk))
      }
    } else {
      var exacts = ac.filter(_.keys.contains(kk))
      if (exacts.size > 1) {
        // different settings of key might override depending on how the key is set
        val sorted = exacts.map(a => (a, a.priority(kk))).sortBy(-_._2)
        if (sorted(1)._2 == sorted.head._2) {
          errors_.update(Some(new Exception(s"Multiple commands with same key ${sorted.map(_._1.description)}")))
        }
        exacts = Seq(sorted.head._1)
      }
      exacts.headOption match {
        case Some(exact) =>
          buffer.append(Part.IdentifiedCommand(Some(kk), exact, ac.filter(a => a.keys.exists(k => k != kk && k.startsWith(kk)))))
        case None =>
          if (removeForStrong != null) {
            buffer.append(removeForStrong)
            tryComplete(false)
            clearPreviousCommand()
            parseCommand(key, false)
          } else {
            buffer.append(Part.UnidentifiedCommand(kk, ac))
          }
      }
    }
  }


  def runTextualIfAvailable(command: Command): Unit = {
    if (command.available(state, this)) {
      clearPreviousCommand()
      buffer.append(IdentifiedCommand(None, command, Seq.empty))
      tryComplete(false)
      commandBufferUpdates_.onNext(buffer)
    }
  }

  private var scheduledComplete: Cancelable = null


  def flushBeforeMouseDown(): Unit = {
    if (scheduledComplete != null) {
      tryComplete(false)
    }
  }

  /**
    * boolean means this command is accepted
    */
  private def tryComplete(wfs: Boolean): Boolean = {
    if (scheduledComplete != null) {
      scheduledComplete.cancel()
      scheduledComplete = null
    }
    def rec(waitForStrong: Boolean): Boolean = {
      val count = buffer.map {
        case Part.Count(c) => c
        case _ => 1
      }.product

      val commands = buffer.flatMap {
        case i: Part.IdentifiedCommand => Seq(i)
        case _ => Seq.empty
      }.toSeq

      val char = buffer.flatMap {
        case i: Part.Char => Seq(i.a)
        case _ => Seq.empty
      }.headOption
      (commands, char) match {
        case (Part.IdentifiedCommand(key, c, remaining) +: Nil, None) =>
          if (c.needsStuff) {
            false
          } else if (waitForStrong && remaining.exists(_.strong)) {
            if (scheduledComplete != null) scheduledComplete.cancel()
            scheduledComplete = Observable.delay({
              scheduledComplete = null
              tryComplete(false)
            }).delaySubscription(300.milliseconds).subscribe()
            true
          } else {
            actAndMarkComplete(c, count, key, None, None)
          }
        case (Part.IdentifiedCommand(key, c, _) +: Nil, Some(ch)) =>
          if (c.needsChar) {
            actAndMarkComplete(c, count, key, char, None)
          } else {
            tryMergeThenRec()
          }
        case (Part.IdentifiedCommand(key1, c1, _) +: Part.IdentifiedCommand(key2, c2, _) +: Nil, _) =>
          if (c2.isInstanceOf[Motion] && c2.needsChar && char.isEmpty) {
            true
          } else if (c1.needsMotion && c2.isInstanceOf[Motion]) {
            actAndMarkComplete(c1, count, key1, char, Some(c2.asInstanceOf[Motion]))
          } else {
            tryMergeThenRec()
          }
        case _ => false
      }
    }

    def tryMergeThenRec(): Boolean = {
      def tryMerge(key1: KeySeq, r1: Seq[Command], key2: KeySeq) = {
        if (key2.size == 1) {
          buffer.remove(buffer.size - 1)
          buffer.remove(buffer.size - 1)
          buffer.append(Part.UnidentifiedCommand(key1, r1))
          parseCommand(key2.head, false)
          rec(false)
        } else {
          markUnknownPattern()
        }
      }
      buffer match {
        case a :+ Part.IdentifiedCommand(Some(key1), c1, r1) :+ Part.UnknownCommand(key2) =>
          tryMerge(key1, r1, key2)
        case a :+ Part.IdentifiedCommand(Some(key1), c1, r1) :+ Part.IdentifiedCommand(Some(key2), c2, _) =>
          tryMerge(key1, r1, key2)
        case _ =>
          markUnknownPattern()
      }
    }
    if (buffer.exists {
      case Part.UnknownCommand(u) => true
      case _ => false
    }) {
      tryMergeThenRec()
    } else if (buffer.exists {
      case Part.UnidentifiedCommand(_, _) => true
      case _ => false
    }) {
      true
    } else {
      rec(wfs)
    }

    // command
    // command char
    // command command char
  }
  private def markUnknownPattern(): Boolean = {
    buffer.append(Part.UnknownPatternMark)
    false
  }

  private def actAndMarkComplete(c: Command, count: Int, key: Option[KeySeq], char: Option[Unicode], motion: Option[Motion]): Boolean = {
    if (char.isDefined) {
      c match {
        case command: FindCommand =>
          lastFindCommand_ = (command, char.get)
        case _ =>
      }
      motion match {
        case Some(command: FindCommand) =>
          lastFindCommand_ = (command, char.get)
        case _ =>
      }
    }
    val res = c.action(state, count, this, key, char, motion)
    buffer.append(Part.CompleteMark)
    localChange(res)
    !c.emptyAsFalseInInsertMode || res != DocTransaction.empty
  }


  def onDoubleClick(): Unit = {
    val avs = commands.filter(c => c.actDoubleClick && c.available(state, this)).sortBy(-_.priority(Seq.empty))
    avs.headOption.foreach(c => runTextualIfAvailable(c))
  }

  private def clearPreviousCommand(): Unit = {
    buffer.lastOption match {
      case Some(_: Part.Finished) => buffer.clear()
      case _ =>
    }
  }

  private var isInsertOverride = false

  protected def keyDown(key: Key): Boolean = {
    // always reset register on any key event
    isInsertOverride = false
    val bufferBefore = if (key.control || key.meta) buffer.clone() else null
    if (!enableModal || state.isInsert) {
      if (key.isSimpleGrapheme) {
        if ((enableModal && !state.isInsert) || !insertModeCommands.exists(_.maybeInsertModeGrapheme(key.a.asInstanceOf[Key.Grapheme].a))) {
          if (scheduledComplete != null) {
            tryComplete(false)
          }
          return false
        } else {
          isInsertOverride = true
        }
      }
    }
    clearPreviousCommand()
    buffer.lastOption match {
      case Some(Part.IdentifiedCommand(k, c, _)) if c.needsChar =>
        key.a match {
          case Key.Grapheme(g) => buffer.append(Part.Char(g))
          case _ =>
            if (miscCommands.exit.keys.contains(Seq(key))) {
              buffer.append(Part.IdentifiedCommand(Some(Seq(key)), miscCommands.exit, Seq.empty))
            } else {
              buffer.append(Part.UnknownCommand(Seq(key)))
            }
        }
      case Some(Part.Count(c)) =>
        key.a match {
          case Key.Grapheme(a) if a.isDigit =>
            buffer.remove(buffer.size - 1)
            buffer.append(Part.Count(c * 10 + a.asDigit))
          case _ => parseCommand(key)
        }
      case Some(Part.UnidentifiedCommand(_, _)) =>
        parseCommand(key)
      case _ =>
        if (state.isRichInsert) {
          parseCommand(key)
          buffer.headOption match {
            case Some(Part.IdentifiedCommand(_, _, _)) =>
            case _ => return false
          }
        } else {
          key.a match {
            case Key.Grapheme(a) if a.isDigit && a.asDigit > 0 =>
              buffer.append(Part.Count(a.asDigit))
            case _ => parseCommand(key)
          }
        }
    }

    val hasExit = buffer.exists {
      case IdentifiedCommand(k, c, _) if c == miscCommands.exit => true
      case _ => false
    }
    val res = tryComplete(true)
    if (hasExit) buffer.clear()
    val ak = hasExit || res || (enableModal && !state.isRichInsert && !key.meta && !key.control)
    if (justSet) {
      justSet = false
    } else {
      if (hasExit || res) {
        setRegister(-1)
      }
    }
    if (!ak && bufferBefore != null) {
      buffer.clear()
      buffer.appendAll(bufferBefore)
      return false
    } else {
      commandBufferUpdates_.onNext(buffer)
    }
    ak
  }

}
