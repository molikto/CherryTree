package command.defaults

import client.Client
import client.Client.ViewMessage
import command._
import command.Key._
import doc.{DocState, DocTransaction}
import model.data.{apply => _, _}
import model.mode
import model.range.IntRange

import scala.util.{Success, Try}

class Misc(val handler: CommandHandler) extends CommandCategory("misc") {


  // what/s these?
  //
  // i_<Esc>       <Esc>             end Insert mode, back to Normal mode
  // i_CTRL-C      CTRL-C            like <Esc>, but do not use an abbreviation
  // i_CTRL-O      CTRL-O {command}  execute {command} and return to Insert mode

  val exit: Command = new Command  with NonConfigurableCommand {
    override val description: String = "exit current mode"
    override val hardcodeKeys: Seq[KeySeq] = Seq(Escape)

    override def available(a: DocState, commandState: CommandInterfaceAvailable): Boolean = true


    override protected def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      a.mode match {
        case Some(m) =>
          m match {
            case model.mode.Node.Visual(fix, move) =>
              DocTransaction(model.data.Node.defaultMode(a.node, move, enableModal))
            case nc@model.mode.Node.Content(n, c) =>
              a.node(n).content match {
                case model.data.Content.Rich(rich) =>
                  c match {
                    case model.mode.Content.RichInsert(pos) =>
                      if (enableModal) {
                        val range = rich.rangeBefore(pos)
                        DocTransaction(a.copyContentMode(model.mode.Content.RichNormal(range)))
                      } else {
                        DocTransaction.empty
                      }
                    case model.mode.Content.RichVisual(_, move) =>
                      if (enableModal) {
                        DocTransaction(a.copyContentMode(model.mode.Content.RichNormal(move)))
                      } else {
                        DocTransaction.empty
                      }
                    case _ => DocTransaction.empty
                  }
                case model.data.Content.Code(_, _) =>
                  c match {
                    case i:model.mode.Content.CodeInside =>
                      DocTransaction(a.copyContentMode(model.mode.Content.CodeNormal))
                    case _ => DocTransaction.empty
                  }
                case _ =>
                  DocTransaction.empty
              }
          }
        case None => DocTransaction.empty
      }
    }

  }

  new Command {
    override val description: String = "quick search in current viewport"

    override def defaultKeys: Seq[KeySeq] = Seq(ModKey + "o")

    override protected def available(a: DocState): Boolean = true

    override def action(a: DocState, count: Int, commandState: CommandInterface, key: Option[KeySeq], grapheme: Option[Unicode], motion: Option[Motion]): DocTransaction = {
      DocTransaction.message(Client.ViewMessage.QuickSearch(true))
    }
  }

  new Command {
    override val description: String = "quick search in whole document"

    override def defaultKeys: Seq[KeySeq] = Seq(shiftMod("o"))

    override protected def available(a: DocState): Boolean = true

    override def action(a: DocState, count: Int, commandState: CommandInterface, key: Option[KeySeq], grapheme: Option[Unicode], motion: Option[Motion]): DocTransaction = {
      DocTransaction.message(Client.ViewMessage.QuickSearch(false))
    }
  }


  val commandMenu = new Command {
    override protected def available(a: DocState): Boolean = a.mode.nonEmpty

    override val description: String = "show contextual command menu"
    override def defaultKeys: Seq[KeySeq] = Seq(":", Key.Ctrl + ";")

    override def action(a: DocState, count: Int, commandState: CommandInterface, key: Option[KeySeq], grapheme: Option[Unicode], motion: Option[Motion]): DocTransaction = {
      DocTransaction.message(Client.ViewMessage.ShowCommandMenu())
    }
  }

  // LATER we only support url and title attribute now, no need to worry about future
  new Command {
    override val description: String = "edit link/image attributes"
    override def defaultKeys: Seq[KeySeq] = Seq(Enter)
    override def priority: Int = 2
    override def available(a: DocState): Boolean = a.isRich((cur, rich, t) => {
      rich.insideUrlAttributed(t.range.until).nonEmpty
    })
    override def actDoubleClick: Boolean = !enableModal
    override def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      val (cur, rich, t) = a.asRichAtom
      val text = rich.insideUrlAttributed(t.range.until).get
      a.editAttribute(text)
    }
  }

  new Command {
    override val description: String = "edit source code"
    override def defaultKeys: Seq[KeySeq] = Seq(Enter)
    override def priority: Int = 2
    override def available(a: DocState): Boolean = a.isRich((cur, rich, t) => {
      t.text.isCodedAtomic
    }) || a.isCodeNormal
    override def actDoubleClick: Boolean = !enableModal
    override def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      if (a.isCodeNormal) {
        DocTransaction(a.copyContentMode(mode.Content.CodeInside(if (enableModal) "normal" else "insert", 0)))
      } else {
        val (_, _, t) = a.asRichAtom
        a.editCode(t, enableModal)
      }
    }
  }


  val visitLink = new Command {
    override val description: String = "visit link destination"

    override def defaultKeys: Seq[KeySeq] = Seq("gx")

    override def available(a: DocState): Boolean = a.isRich((cursor, rich, t) => {
      rich.befores(t.range.until).exists(a => a.isStartWithAttribute(UrlAttribute) && a.textRange.contains(t.range))
    })
    override def actDoubleClick: Boolean = !enableModal

    override def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      val (_, rich, t0) = a.asRichAtom
      val t = rich.befores(t0.range.until).find(_.isStartWithAttribute(UrlAttribute)).get.text.asDelimited
      val url = t.attribute(model.data.UrlAttribute).str
      Node.matchNodeRef(url).foreach { uuid =>
        a.lookup(uuid) match {
          case Some(cur) =>
            return a.goTo(cur, Misc.this)
          case None =>
            // TODO report error
            return DocTransaction.empty
        }
      }
      import io.lemonlabs.uri._
      Try {
        Url.parse(url)
      } match {
        case Success(_) =>
          DocTransaction.message(Client.ViewMessage.VisitUrl(url))
        case _ =>
          DocTransaction.empty
      }
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



  // Q_vm          Various motions
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

