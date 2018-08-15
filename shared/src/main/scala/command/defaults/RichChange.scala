package command.defaults

import client.Client
import command.{CommandCategory, CommandInterface, Motion}
import command.Key._
import doc.{DocState, DocTransaction}
import model.data.{apply => _, _}
import model.range.IntRange
import model.{cursor, mode, operation}

class RichChange extends CommandCategory("change text") {

  new Command {
    override val description: String = "delete all and go to insert"
    override def defaultKeys: Seq[KeySeq] = Seq("cc", "S")
    override protected def available(a: DocState): Boolean = a.isRichNormal
    override protected def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      val (cursor, _, _) = a.asRichNormal
      DocTransaction(Seq(operation.Node.Replace(cursor, model.data.Content.Rich(model.data.Rich.empty))),
        Some(a.copyContentMode(mode.Content.RichInsert(0))))
    }
  }

  new Command {
    override val description: String = "change selected text"
    override val defaultKeys: Seq[KeySeq] = Seq("c")
    override def available(a: DocState): Boolean = a.isRichVisual
    override def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = a.mode match {
      case Some(model.mode.Node.Content(pos, v@model.mode.Content.RichVisual(_, _))) =>
        deleteRichNormalRange(a, commandState,pos, v.merged, insert = true)
      case _ => throw new IllegalArgumentException("Invalid command")
    }
  }


  new Command {
    override val description: String = "change range selected by motion"
    override def needsMotion: Boolean = true
    override val defaultKeys: Seq[KeySeq] = Seq("c")
    override protected def available(a: DocState): Boolean = a.isRichNormal

    override def action(a: DocState, count: Int, commandState: CommandInterface, key: Option[KeySeq], grapheme: Option[Unicode], motion: Option[Motion]): DocTransaction = {
      val (cur, rich, normal) = a.asRichNormal
      motion.flatMap(m => {
        m.act(commandState, rich, count, normal.range, grapheme).map(r => {
          deleteRichNormalRange(a, commandState,cur, r, insert = true)
        })
      }).getOrElse(DocTransaction.empty)
    }
  }



  new NeedsCharCommand {
    override val description: String = "change char under the cursor"
    override def defaultKeys: Seq[KeySeq] = Seq("r") // DIFFERENCE command merged, also not avaliable in visual node mode, only single char accepted now
    override def available(a: DocState): Boolean = a.isRichNormal((_, p) => !p.special)

    override def action(a: DocState, count: Int, commandState: CommandInterface, key: Option[KeySeq], grapheme: Option[Unicode], motion: Option[Motion]): DocTransaction = {
      if (grapheme.isEmpty) return DocTransaction.empty
      val char = grapheme.get
      val (cursor, rich, v) = a.asRichNormal
      if (rich.isEmpty) return DocTransaction(a.copyContentMode(mode.Content.RichInsert(0)))

      def makeMode(in: Atom, riches: Seq[operation.Rich]): Option[mode.Node] = {
        val rafter = operation.Rich.apply(riches, rich)
        val range = if (in.delimitationStart) {
          rafter.after(in.textTotalIndex).range
        } else {
          rafter.before(in.textTotalIndex + rafter.after(in.textTotalIndex).text.size).range
        }
        Some(a.copyContentMode(mode.Content.RichNormal(range)))
      }
      if (!rich.after(v.range.start).special) {
        val ops = operation.Rich.replacePlain(v.range.start, v.range.until, char)
        val focus = IntRange(v.range.start, v.range.start + char.size)
        DocTransaction(Seq(operation.Node.rich(cursor, ops)),
          Some(a.copyContentMode(mode.Content.RichNormal(focus))))
      } else {
        DocTransaction.empty
      }
    }

  }

  new Command {
    override val description: String = "change text cursor until text end"
    override def defaultKeys: Seq[KeySeq] = Seq("C")
    override def available(a: DocState): Boolean = a.isRichNormal
    override def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      val (c, rich, normal) = a.asRichNormal
      deleteRichNormalRange(a, commandState,c, IntRange(normal.range.start, rich.size), insert = true)
    }
  }

  new Command {
    override def repeatable: Boolean = true
    override val description: String = "change under cursor, and more after if has N"
    override val defaultKeys: Seq[KeySeq] = Seq("s")
    override def available(a: DocState): Boolean = a.isRichNormal
    override def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      val (pos, rich, normal) = a.asRichNormal
      val r = normal.range
      val fr = (1 until count).foldLeft(r) {(r, _) => if (r.until == rich.size) r else rich.rangeAfter(r) }
      deleteRichNormalRange(a, commandState,pos, r.merge(fr), insert = true)
    }

  }

  //      val replace: Command = new Command {
  //        override def defaultKeys: Seq[KeySeq] = Seq("gr", "r") // DIFFERENCE command merged, also not avaliable in visual node mode
  //        override def available(a: ClientState): Boolean = a.isRichNormalOrVisual
  //        override def action(a: ClientState, count: Int): DocTransaction = {
  //          waitingForCharCommand = (this, count)
  //          DocTransaction.empty
  //        }
  //
  //
  //        override def actionOnGrapheme(a: ClientState, char: Grapheme, count: Int): DocTransaction = {
  //          val (cursor, rich, v) = a.asRichNormalOrVisual
  //          def makeMode(in: Info, riches: Seq[operation.Rich]): Option[mode.Node] = {
  //            val rafter = operation.Rich.apply(riches, rich)
  //            val range = if (in.isStart) {
  //              rafter.info(in.nodeStart).atomicRange
  //            } else {
  //              rafter.info(in.nodeStart + rafter.info(in.nodeStart).text.size - 1).atomicRange
  //            }
  //            Some(a.copyContentMode(mode.Content.RichNormal(range)))
  //          }
  //          if (v.merged.si== 1 && char.a.si == 1 && count == 1) {
  //            val point = v.merged.start
  //            val in = rich.info(point)
  //            if (in.isStartOrEnd) {
  //              val codepoint = char.a.codePoints.head
  //              delimitationSettings.find(_._2 == codepoint) match {
  //                case Some(deli) =>
  //                  if (in.specialChar == deli._1.start || in.specialChar == deli._1.end) {
  //                    return DocTransaction.empty
  //                  }
  //                  def wrapUnwrap(): DocTransaction = {
  //                    val op1 = operation.Rich.unwrap(in.nodeStart, in.text.asInstanceOf[Text.Delimited[Any]])
  //                    val op2 = operation.Rich.wrap(IntRange(in.nodeStart, in.nodeStart + in.text.asInstanceOf[Text.Coded].contentSize), deli._1)
  //                    DocTransaction(
  //                      Seq(
  //                        operation.Node.Content(cursor, operation.Content.Rich(operation.Rich.merge(op1, op2, operation.Type.AddDelete)))
  //                      ),
  //                      makeMode(in, Seq(op1, op2))
  //                    )
  //                  }
  //                  if (SpecialChar.coded.contains(deli)) {
  //                    if (in.text.asInstanceOf[Text.Formatted].content.sie == 1 &&  in.text.asInstanceOf[Text.Formatted].content.head.isPlain) {
  //                      val unicode = in.text.asInstanceOf[Text.Formatted].content.head.asInstanceOf[Text.Plain].unicode
  //                      val op1 = operation.Rich.unwrap(in.nodeStart, in.text.asInstanceOf[Text.Delimited[Any]])
  //                      val op2 = operation.Rich.wrapAsCoded(unicode, IntRange(in.nodeStart, in.nodeStart + unicode.size), deli._1)
  //                      return DocTransaction(
  //                        Seq(
  //                          operation.Node.Content(cursor, operation.Content.Rich(operation.Rich.merge(op1, op2, operation.Type.AddDelete)))
  //                        ),
  //                        makeMode(in, Seq(op1, op2))
  //                      )
  //                    } else if (in.text.isCoded) {
  //                      return wrapUnwrap()
  //                    }
  //                  } else if (SpecialChar.formatLike.contains(deli) || SpecialChar.linkLike.contains(deli)) {
  //                    return wrapUnwrap()
  //                  }
  //                case None =>
  //              }
  //            }
  //          }
  //          val merged = v match {
  //            case mode.Content.RichNormal(r) => IntRange(r.start, (0 until count).foldLeft(r) { (rr, _) => rich.moveRightAtomic(r) }.until)
  //            case v: mode.Content.RichVisual => v.merged
  //          }
  //          val notPlain = rich.info(merged).filter(_.ty != InfoType.Plain).map(a => IntRange(a.positionInParagraph))
  //          val plains = merged.minusOrderedInside(notPlain)
  //          if (plains.isEmpty) {
  //            DocTransaction.empty
  //          } else {
  //            val gcs = plains.map(a => rich.subPlain(a)).map(a => (a, a.graphemesCount))
  //            println(plains)
  //            println(gcs)
  //            val newSize = gcs.map(_._2).sum * char.a.size
  //            val oldSize = gcs.map(_._1.size).sum
  //            val ops = operation.Rich.merge(plains.zip(gcs).reverse.flatMap(r => Seq(
  //              operation.Rich.delete(r._1),
  //              operation.Rich.insert(r._1.start, char.a.times(r._2._2))
  //            )), operation.Type.AddDelete)
  //            val focus = if (v.isNormal || v.focus.start == merged.start) {
  //              if (plains.head.start == merged.start) {
  //                IntRange(v.focus.start, v.focus.start + char.a.size) // changed
  //              } else {
  //                v.focus // not changed
  //              }
  //            } else {
  //              val ss = if (plains.last.until == merged.until) {
  //                char.a.size
  //              } else {
  //                v.focus.size
  //              }
  //              val end = v.focus.until + newSize - oldSize
  //              IntRange(end -ss, end)
  //            }
  //
  //            DocTransaction(Seq(operation.Node.Content(cursor, operation.Content.Rich(ops))),
  //              Some(a.copyContentMode(mode.Content.RichNormal(focus))))
  //          }
  //        }
  //      }

  // LATER remaining changes, some might can be implemented
  // R       N  R          enter Replace mode (repeat the entered text N times)
  //gR      N  gR         enter virtual Replace mode: Like Replace mode but
  //                           without affecting layout
  //v_b_r      {visual}r{char}
  //                        in Visual block mode: Replace each char of the
  //                           selected text with {char}
  //
  //v_b_c      {visual}c  in Visual block mode: Change each of the selected
  //                           lines with the entered text
  //v_b_C      {visual}C  in Visual block mode: Change each of the selected
  //                           lines until end-of-line with the entered text
  //
  //~       N  ~          switch case for N characters and advance cursor
  //v_~        {visual}~  switch case for highlighted text
  //v_u        {visual}u  make highlighted text lowercase
  //v_U        {visual}U  make highlighted text uppercase
  //g~         g~{motion} switch case for the text that is moved over with
  //                           {motion}
  //gu         gu{motion} make the text that is moved over with {motion}
  //                           lowercase
  //gU         gU{motion} make the text that is moved over with {motion}
  //                           uppercase
  //v_g?       {visual}g? perform rot13 encoding on highlighted text
  //g?         g?{motion} perform rot13 encoding on the text that is moved over
  //                           with {motion}
  //
  //CTRL-A  N  CTRL-A     add N to the number at or after the cursor
  //CTRL-X  N  CTRL-X     subtract N from the number at or after the cursor
  //
  //<       N  <{motion}  move the lines that are moved over with {motion} one
  //                           shiftwidth left
  //<<      N  <<         move N lines one shiftwidth left
  //>       N  >{motion}  move the lines that are moved over with {motion} one
  //                           shiftwidth right
  //>>      N  >>         move N lines one shiftwidth right
  //gq      N  gq{motion} format the lines that are moved over with {motion} to
  //                           'textwidth' length
  //:ce     :[range]ce[nter] [width]
  //                        center the lines in [range]
  //:le     :[range]le[ft] [indent]
  //                        left-align the lines in [range] (with [indent])
  //:ri     :[range]ri[ght] [width]
  //                        right-align the lines in [range]

  // not implemented
  // Q_co          Complex changes
  //
  //!        N  !{motion}{command}<CR>
  //                        filter the lines that are moved over through {command}
  //!!       N  !!{command}<CR>
  //                        filter N lines through {command}
  //v_!         {visual}!{command}<CR>
  //                        filter the highlighted lines through {command}
  //:range!  :[range]! {command}<CR>
  //                        filter [range] lines through {command}
  //=        N  ={motion}
  //                        filter the lines that are moved over through 'equalprg'
  //==       N  ==        filter N lines through 'equalprg'
  //v_=         {visual}=
  //                        filter the highlighted lines through 'equalprg'
  //:s       :[range]s[ubstitute]/{pattern}/{string}/[g][c]
  //                        substitute {pattern} by {string} in [range] lines;
  //                           with [g], replace all occurrences of {pattern};
  //                           with [c], confirm each replacement
  //:s       :[range]s[ubstitute] [g][c]
  //                        repeat previous ":s" with new range and options
  //&           &         Repeat previous ":s" on current line without options
  //:ret     :[range]ret[ab][!] [tabstop]
  //                        set 'tabstop' to new value and adjust white space
  //                           accordingly
}
