package command.defaults

import client.Client
import command.CommandCollector
import command.Key._
import model.ClientState
import model.range.IntRange

trait Misc extends CommandCollector {


  val exit: Command = new Command {
    override val defaultKeys: Seq[KeySeq] = Seq(Escape, Ctrl + "c", Ctrl + "[")
    override def available(a: ClientState): Boolean = true

    override def action(a: ClientState, ignore: Int): Client.Update = {
      a.mode match {
        case Some(m) =>
          m match {
            case model.mode.Node.Visual(fix, move) =>
              Client.Update.mode(model.data.Node.defaultNormalMode(a.node, move))
            case nc@model.mode.Node.Content(n, c) =>
              a.node(n).content match {
                case model.data.Content.Rich(rich) =>
                  c match {
                    case model.mode.Content.RichInsert(pos) =>
                      val range = if (pos != 0) rich.moveLeftAtomic(pos)
                      else if (rich.isEmpty) IntRange(0, 0)
                      else rich.beginningAtomicRange()
                      Client.Update.mode(a.copyContentMode(model.mode.Content.RichNormal(range)))
                    case model.mode.Content.RichVisual(_, move) =>
                      Client.Update.mode(a.copyContentMode(model.mode.Content.RichNormal(move)))
                    case _ => Client.Update.empty
                  }
                case model.data.Content.Code(_, _) =>
                  c match {
                    case model.mode.Content.CodeInside =>
                      Client.Update.mode(a.copyContentMode(model.mode.Content.CodeNormal))
                    case _ => Client.Update.empty
                  }
                case _ =>
                  Client.Update.empty
              }
          }
        case None => Client.Update.empty
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

