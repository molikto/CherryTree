package command.defaults

import command.{CommandCategory, CommandInterface, Key, Motion}
import client.Client
import client.Client.ViewMessage
import command.Key._
import doc.{DocState, DocTransaction}
import model.data.{apply => _, _}
import model.range.IntRange
import model.{cursor, data, mode, operation}

class RichSpecial extends CommandCategory("text format") {


  SpecialChar.all.map(deli => deli -> new DeliCommand(deli) {

    override val description: String = if (deli.atomic) s"insert a new ${deli.name}" else  s"insert a new/or move cursor out of ${deli.name}"

    override def emptyAsFalseInInsertMode: Boolean = true

    override def available(a: DocState): Boolean =
      a.isRichNormalOrInsert && {
        val (node, rich, insert, until) = a.asRichNormalOrInsert
        val op1 = if (deli.coded) {
          if (rich.insideCoded(insert)) {
            if (rich.insideCoded(insert, deli)) {
              !rich.wrappedByCodedContent(insert)
            } else {
              false
            }
          } else {
            true
          }
        } else {
          !rich.insideCoded(insert)
        }
        op1 && !a.isRichNormal((_, p) => p.special)
      }

    override def action(a: DocState, count: Int, commandState: CommandInterface, key: Option[KeySeq], grapheme: Option[Unicode], motion: Option[Motion]): DocTransaction = {
      val (n, content, insert, until) = a.asRichNormalOrInsert
      val keyU = Unicode(key.map(a => Key.toString(a)).getOrElse(""))
      def moveSomeInsertMode(some: Int) = Some(a.copyContentMode(mode.Content.RichInsert(insert + some)))
      if (insert < content.size && content.after(insert).special(deli.end) && key.nonEmpty && delimitationGraphemes.get(deli.end).contains(keyU)) {
        DocTransaction(Seq.empty, moveSomeInsertMode(1))
      } else if (!content.insideCoded(insert) && (key.isEmpty || delimitationGraphemes.get(deli.start).contains(keyU))) {
        val wrap = deli.wrap()
        val k = operation.Rich.insert(insert, wrap)
        val trans = Seq(model.operation.Node.rich(n, k))
        if (deli.atomic) {
          DocTransaction(trans, moveSomeInsertMode(wrap.size),
            viewMessagesAfter = Seq(ViewMessage.ShowUrlAndTitleAttributeEditor(n, IntRange(insert, insert + wrap.size), Text.Image(Unicode.empty))))
        } else {
          DocTransaction(trans, moveSomeInsertMode(1))
        }
      } else {
        DocTransaction.empty
      }
    }

    override protected def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = throw new NotImplementedError("Should not call this")
  }).toMap


  SpecialChar.all.map(deli => deli -> new DeliCommand(deli) {

    override val description: String = s"change to a ${deli.name}"

    override def available(a: DocState): Boolean = a.isRichNormal((_, p) => p.special)

    override def action(a: DocState, count: Int, commandState: CommandInterface, key: Option[KeySeq], grapheme: Option[Unicode], motion: Option[Motion]): DocTransaction = {
      val (cursor, content, in) = a.asRichNormalAtom
      if (key.isEmpty) return DocTransaction.empty
      if (in.asInstanceOf[Atom.Special].delimitation == deli) return DocTransaction.empty


      def resMode(): Option[mode.Node] = {
        Some(
          if (in.delimitationStart) a.mode.get
          else a.copyContentMode(mode.Content.RichNormal(IntRange.len(in.range.start, deli.newDeliEndSize)))
        )
      }
      def wrapUnwrap(): DocTransaction = {
        val op1 = operation.Rich.unwrap(in.textTotalIndex, in.text.asDelimited)
        val op2 = operation.Rich.wrap(IntRange(in.textTotalIndex, in.textTotalIndex + in.text.asDelimited.contentSize), deli)
        DocTransaction(
          Seq(
            operation.Node.rich(cursor, operation.Rich.merge(op1, op2, operation.Type.AddDelete))
          ), resMode()
        )
      }

      if (SpecialChar.coded.contains(deli)) {
        in.text match {
          case formatted: Text.Formatted if formatted.content.size == 1 && formatted.content.head.isPlain =>
            val unicode = formatted.content.head.asPlain.unicode
            val op1 = operation.Rich.unwrap(in.textTotalIndex, in.text.asDelimited)
            val op2 = operation.Rich.wrapAsCoded(unicode, IntRange(in.textTotalIndex, in.textTotalIndex + unicode.size), deli)
            return DocTransaction(
              Seq(
                operation.Node.rich(cursor, operation.Rich.merge(op1, op2, operation.Type.AddDelete))
              ),
              resMode()
            )
          case _ => if (in.text.isCoded) {
            return wrapUnwrap()
          }
        }
      } else if (SpecialChar.formattedSplittable.contains(deli) || SpecialChar.formattedNonSplittable.contains(deli)) {
        return wrapUnwrap()
      }
      return DocTransaction.empty
    }

    override protected def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = throw new NotImplementedError("Should not call this")
  }).toMap


  abstract class WrapCommand(deli: SpecialChar.Delimitation) extends DeliCommand(deli) {
    override def available(a: DocState): Boolean = a.isRichVisual || {
      if (a.isNodeVisual) {
        val v = a.asNodeVisual
        if (v.fix == v.move) {
          val node = a.node(v.fix)
          node.content.isRich && node.childs.isEmpty
        } else {
          false
        }
      } else {
        false
      }
    }
  }

  SpecialChar.formattedSplittable.map(deli => deli -> new WrapCommand(deli) {
    override val description: String = s"wrap selection in ${deli.name}"
    override def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction =
      if (a.isRichVisual) {
        a.asRichVisual match {
          case (cursor, rich, visual) =>
            val r = visual.merged
            val after = rich.after(r.start)
            if (after.special(deli.start) && rich.before(r.until).special(deli.end)) {
              val ret = if (visual.fix.start > visual.move.start) {
                mode.Content.RichVisual(rich.before(visual.fix.start).range.moveBy(-deli.newDeliStartSize),
                  rich.after(visual.move).range.moveBy(-deli.newDeliStartSize))
              } else {
                mode.Content.RichVisual(rich.after(visual.fix).range.moveBy(-deli.newDeliStartSize),
                  rich.before(visual.move.start).range.moveBy(-deli.newDeliStartSize))
              }
              DocTransaction(Seq(operation.Node.rich(cursor,
                operation.Rich.unwrap(r.start, after.text.asDelimited))),
                Some(a.copyContentMode(ret)))
            } else {
              val soc = rich.singleSpecials(r).map(_.range)
              val remaining = r.minusOrderedInside(soc)
              val range = (r.start, r.until + remaining.size * deli.wrapSizeOffset - 1)
              val fakePoints = if (visual.fix.start <= visual.move.start) {
                mode.Content.RichVisual(IntRange.len(range._1, deli.newDeliStartSize), IntRange.endLen(range._2 + deli.newDeliStartSize, deli.newDeliEndSize))
              } else {
                mode.Content.RichVisual(IntRange.endLen(range._2 + deli.newDeliStartSize, deli.newDeliEndSize), IntRange.len(range._1, deli.newDeliStartSize))
              }
              DocTransaction(Seq(operation.Node.rich(cursor,
                operation.Rich.wrapNonOverlappingOrderedRanges(remaining, deli))),
                Some(a.copyContentMode(fakePoints)))
            }
        }
      } else if (a.isNodeVisual) {
        val v = a.asNodeVisual
        if (v.fix == v.move) {
          val node = a.node(v.fix)
          if (node.content.isRich && node.childs.isEmpty) {
            val rich = node.content.asInstanceOf[data.Content.Rich].content
            if (rich.text.size == 1 && rich.text.head.isDelimited && rich.text.head.asDelimited.delimitation == deli) {
              return DocTransaction(Seq(operation.Node.rich(v.fix, operation.Rich.deleteOrUnwrapAt(rich, 0))), None)
            } else {
              return DocTransaction(Seq(operation.Node.rich(v.fix, operation.Rich.wrap(IntRange(0, rich.size), deli))), None)
            }
          }
        }
        DocTransaction.empty
      } else {
        DocTransaction.empty
      }
  }).toMap

  /**
    * code wrap CANNOT use surround!!! because this breaks cursor placement and might insert special char inside code
    * also we currently only wraps plain text
    */
  SpecialChar.coded.map(deli => deli -> new WrapCommand(deli) {
    override val description: String = s"wrap selection as ${deli.name}"
    override def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = a.asRichVisual match {
      case (cursor, rich, visual) =>
        val r = visual.merged
        val p = IntRange(r.start, r.until + deli.wrapSizeOffset)
        val fakeMode =
          if (deli.atomic) {
            mode.Content.RichVisual(p, p)
          } else if (visual.fix.start <= visual.move.start) {
            mode.Content.RichVisual(IntRange.len(p.start, deli.newDeliStartSize), IntRange.endLen(p.until, deli.newDeliEndSize))
          } else {
            mode.Content.RichVisual(IntRange.endLen(p.until, deli.newDeliEndSize), IntRange.len(r.start, deli.newDeliStartSize))
          }
        val ifs = rich.between(r)
        if (ifs.forall(_.isInstanceOf[Atom.PlainGrapheme])) {
          DocTransaction(Seq(
            operation.Node.rich(cursor,
              operation.Rich.wrapAsCoded(rich.subPlain(r), r, deli))),
            Some(a.copyContentMode(fakeMode)))
        } else {
          DocTransaction.empty
        }
    }

  }).toMap

  SpecialChar.formattedNonSplittable.map(deli => deli -> new WrapCommand(deli) {
    override val description: String = s"wrap selection in ${deli.name}"
    override def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = a.asRichVisual match {
      case (cursor, rich, visual) =>
        var r = visual.merged
        val g = rich.isSubRich(r)
        if (g.isDefined) r = g.get
        if (g.isDefined) {
          val p = IntRange(r.start, r.until + deli.wrapSizeOffset)
          val fakeMode =
            if (visual.fix.start <= visual.move.start) {
              mode.Content.RichVisual(IntRange.len(p.start, deli.newDeliStartSize), IntRange.endLen(p.until, deli.newDeliEndSize))
            } else {
              mode.Content.RichVisual(IntRange.endLen(p.until, deli.newDeliEndSize), IntRange.len(r.start, deli.newDeliStartSize))
            }
          DocTransaction(Seq(
            operation.Node.rich(cursor, operation.Rich.wrap(r, deli))),
            Some(a.copyContentMode(fakeMode)))
        } else {
          DocTransaction.empty
        }
    }
  }).toMap
}
