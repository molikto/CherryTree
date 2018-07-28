package command

import client.Client
import command.Key._
import command.Part.IdentifiedCommand
import doc.{DocState, DocTransaction}
import model.data.{SpecialChar, Unicode}
import model.range.IntRange
import monix.reactive.Observable
import monix.reactive.subjects._
import register.{RegisterHandler, Registerable}
import settings.Settings

import scala.collection.mutable.ArrayBuffer
import scala.util.{Success, Try}

abstract class CommandHandler extends Settings with CommandInterface {
  self: Client =>


  private val miscCommands = new defaults.Misc()
  val commands: Seq[Command] = Seq(
    miscCommands,
    new defaults.RichMotion(),
    new defaults.RichTextObject(),
    new defaults.RichInsertEnter(),
    new defaults.RichInsert(),
    new defaults.RichVisual(),
    new defaults.RichChange(),
    new defaults.RichDelete(),
    new defaults.NodeMotion(),
    new defaults.NodeVisual(),
    new defaults.NodeMove(),
    new defaults.NodeDelete(),
    new defaults.YankPaste(),
    new defaults.UndoRedo(),
    new defaults.NodeFold(),
    new defaults.Scroll()).flatMap(_.commands)

  val commandsByCategory: Map[String, Seq[Command]] = commands.groupBy(_.category)

  //  private val status =  BehaviorSubject[CommandStatus](CommandStatus.Empty)
  //  def commandStatus: Observable[CommandStatus] = status

  private val buffer = new ArrayBuffer[Part]()

  private val commandBufferUpdates_ = BehaviorSubject[Seq[Part]](buffer)

  def commandBufferUpdates: Observable[Seq[Part]] = commandBufferUpdates_

  private var lastFindCommand_ : (FindCommand, Unicode) = null

  override def lastFindCommand: Option[(FindCommand, Unicode)] = Option(lastFindCommand_)

  override def commandBuffer: Seq[Part] = buffer

  def onBeforeUpdateUpdateCommandState(state: DocState): Unit = {
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
        case _ =>
      }
    }
    if (!av) {
      buffer.clear()
      commandBufferUpdates_.onNext(buffer)
    }
  }

  // LATER updatable keymap
  private lazy val keyToCommand: Map[Key, Seq[command.Command]] = commands.flatMap(c => c.keys.map(_ -> c)).groupBy(_._1.head).map(a => (a._1, a._2.map(_._2)))

  def parseCommand(key: Key): Unit = {
    val (ks, cs) = buffer.lastOption match {
      case Some(Part.UnidentifiedCommand(kks, ccs)) =>
        buffer.remove(buffer.size - 1)
        (kks, ccs)
      case _ => (Seq.empty, keyToCommand.getOrElse(key, Seq.empty))
    }
    val kk = ks :+ key
    val ac = cs.filter(a => a.keys.exists(_.startsWith(kk)) && a.available(state, this))
    if (ac.isEmpty) {
      buffer.append(Part.UnknownCommand(kk))
    } else {
      var exacts = ac.filter(_.keys.contains(kk))
      if (exacts.size > 1) {
        // different settings of key might override depending on how the key is set
        val sorted = exacts.map(a => (a, a.keyLevel(kk))).sortBy(-_._2)
        if (sorted(1)._2 == sorted.head._2) {
          errors_.update(Some(new Exception("Multiple commands with same key")))
        }
        exacts = Seq(sorted.head._1)
      }
      exacts.headOption match {
        case Some(exact) =>
          buffer.append(Part.IdentifiedCommand(kk, exact, ac.filter(_ != exact)))
        case None =>
          buffer.append(Part.UnidentifiedCommand(kk, ac))
      }
    }
  }



  /**
    * boolean means this command is accepted
    */
  private def tryComplete(): Boolean = {
    def rec() = {
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
        case (Part.IdentifiedCommand(key, c, _) +: Nil, None) =>
          if (c.needsStuff) {
            false
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
          parseCommand(key2.head)
          rec()
        } else {
          markUnknownPattern()
        }
      }
      buffer match {
        case a :+ Part.IdentifiedCommand(key1, c1, r1) :+ Part.UnknownCommand(key2) =>
          tryMerge(key1, r1, key2)
        case a :+ Part.IdentifiedCommand(key1, c1, r1) :+ Part.IdentifiedCommand(key2, c2, _) =>
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
      rec()
    }

    // command
    // command char
    // command command char
  }
  private def markUnknownPattern(): Boolean = {
    buffer.append(Part.UnknownPatternMark)
    true
  }

  private def actAndMarkComplete(c: Command, count: Int, key: KeySeq, char: Option[Unicode], motion: Option[Motion]): Boolean = {
    flush()
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
    val res = c.action(state, count, this, Some(key), char, motion)
    buffer.append(Part.CompleteMark)
    change(res)
    !c.emptyAsFalse || res != DocTransaction.empty
  }

  def keyDown(key: Key): Boolean = {
    buffer.lastOption match {
      case Some(Part.CompleteMark) => buffer.clear()
      case Some(Part.UnknownPatternMark) => buffer.clear()
      case Some(Part.UnknownCommand(_)) => buffer.clear()
      case _ =>
    }
    buffer.lastOption match {
      case Some(Part.IdentifiedCommand(k, c, _)) if c.needsChar =>
        key.a match {
          case Key.Grapheme(g) => buffer.append(Part.Char(g))
          case _ =>
            if (miscCommands.exit.keys.contains(Seq(key))) {
              buffer.append(Part.IdentifiedCommand(Seq(key), miscCommands.exit, Seq.empty))
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
        if (state.isRichInserting) {
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

    if (buffer.exists {
      case IdentifiedCommand(k, c, _) if c == miscCommands.exit => true
      case _ => false
    }) {
      flush()
      change(miscCommands.exit.action(state, 1, this, None, None, None))
      buffer.clear()
      commandBufferUpdates_.onNext(buffer)
      true
    } else {
      val res = tryComplete()
      commandBufferUpdates_.onNext(buffer)
      res || !state.isRichInserting
    }
  }


  new SideEffectingCommand {
    override def category: String = miscCommands.name

    override val description: String = "visit link url"

    override def defaultKeys: Seq[KeySeq] = Seq("gx")

    override def available(a: DocState): Boolean = a.isNonEmptyRichNormalOrVisual && {
      val (_, rich, nv) = a.asRichNormalOrVisual
      if (rich.isEmpty) false
      else {
        val t = rich.after(nv.focus.start)
        t.text.isDelimited && SpecialChar.urlAttributed.contains(t.text.asDelimited.delimitation)
      }
    }

    override def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      val (_, rich, nv) = a.asRichNormalOrVisual
      val t = rich.after(nv.focus.start)
      val url = t.text.asDelimited.attribute(model.data.UrlAttribute).str
      import io.lemonlabs.uri._
      Try {
        Url.parse(url)
      } match {
        case Success(_) => viewMessages_.onNext(Client.ViewMessage.VisitUrl(url))
        case _ =>
      }
      DocTransaction.empty
    }
  }

}
