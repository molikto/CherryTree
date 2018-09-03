package command.defaults

import command._
import client.Client
import client.Client.ViewMessage
import command.Key._
import doc.{DocState, DocTransaction}
import model.data.{apply => _, _}
import model.range.IntRange
import model.{cursor, data, mode, operation}

class RichSpecial extends CommandCategory("rich text: format") {


  SpecialChar.all.map(deli => deli -> new DeliCommand(deli) {

    override val description: String = if (deli.atomic) s"insert a new ${deli.name}" else  s"insert a new/or move cursor out of ${deli.name}"

    override def emptyAsFalseInInsertMode: Boolean = true

    override def priority(key: KeySeq): Int = if (key.isEmpty) 4 else 0

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
        op1 && (keys.isEmpty || !a.isRichNormal((_, p) => p.special))
      }

    override def action(a: DocState, count: Int, commandState: CommandInterface, key: Option[KeySeq], grapheme: Option[Unicode], motion: Option[Motion]): DocTransaction = {
      val (n, content, insert, until) = a.asRichNormalOrInsert
      val keyU = Unicode(key.map(a => if (a.forall(_.isSimpleGrapheme)) Key.toString(a) else "").getOrElse(""))
      def moveSomeInsertMode(some: Int) = mode.Content.RichInsert(insert + some)
      if (insert < content.size && content.after(insert).special(deli.end) && keyU.nonEmpty && delimitationGraphemes.get(deli.end).contains(keyU)) {
        DocTransaction(a.copyContentMode(moveSomeInsertMode(content.after(insert).size)))
      } else if (!content.insideCoded(insert) && (keyU.isEmpty || delimitationGraphemes.get(deli.start).contains(keyU))) {
        val extraInsert =
        if (keyU.isEmpty) {
          None
        } else {
          Some(keyU)
        }
        val k = operation.Rich.insert(insert, deli)
        val trans = extraInsert.map(_ => model.operation.Node.rich(n, operation.Rich.delete(IntRange(insert, insert + keyU.size)))).toSeq :+ operation.Node.rich(n, k)
        val modeBefore = moveSomeInsertMode(if (deli.atomic) deli.emptySize else 1)
        val vms = if (deli == SpecialChar.Image) {
          a.editAttribute(IntRange.len(insert + 1, 0), modeBefore)
        } else if (deli == SpecialChar.LaTeX) {
          a.editCode(IntRange.len(insert + 1, 0), enableModal, modeBefore)
        } else if (deli == SpecialChar.HTML) {
          a.editCode(IntRange.len(insert + 1, 0), enableModal, modeBefore)
        } else {
          DocTransaction(a.copyContentMode(modeBefore))
        }
        val ret = vms.copy(trans)
        extraInsert match {
          case Some(extra) => DocTransaction(
            Seq(model.operation.Node.rich(n, operation.Rich.replacePlain(insert, insert, extra))),
            Some(a.copyContentMode(model.mode.Content.RichInsert(insert + extra.size))),
            extra = Some(ret))
          case None => ret
        }
      } else {
        DocTransaction.empty
      }
    }

    override protected def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = throw new IllegalArgumentException("Should not call this")
  }).toMap



  SpecialChar.nonAtomic.map(deli => deli -> new DeliCommand(deli) {

    override val description: String = s"change to a ${deli.name}"


    override def available(a: DocState, commandState: CommandInterfaceAvailable): Boolean =
      keys.nonEmpty && a.isRichNormal((_, p) => p.special) && !commandState.needsMotion

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
        DocTransaction(
          Seq(
            operation.Node.rich(cursor, operation.Rich.wrapUnwrap(in.textTotalIndex, in.text.asDelimited, deli))
          ), resMode())
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

    override protected def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = throw new IllegalArgumentException("Should not call this")
  }).toMap



  abstract class WrapCommand(deli: SpecialChar.Delimitation) extends DeliCommand(deli) {

    override def emptyAsFalseInInsertMode: Boolean = true

    override def available(a: DocState): Boolean = {
      if (a.isRichVisual) {
        val (node, rich, visual) = a.asRichVisual
        val merge = visual.merged
        !rich.insideCoded(merge.start) && !rich.insideCoded(merge.until)
      } else {
        false
      }
    } || {
      if (a.isNodeVisual) {
        val v = a.asNodeVisual
        if (v.fix == v.move) {
          val node = a.node(v.fix)
          node.content.isRich
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
            val before = rich.before(r.until)
            if (after.special(deli.start) && before.special(deli.end)) {
              var ret = if (visual.fix.start > visual.move.start) {
                mode.Content.RichVisual(rich.before(before.range.start).range.moveBy(-1),
                  rich.after(after.range.until).range.moveBy(-1))
              } else {
                mode.Content.RichVisual(rich.after(after.range.until).range.moveBy(-1),
                  rich.before(before.range.start).range.moveBy(-1))
              }
              ret = ret.maybeEmpty(visual)
              DocTransaction(Seq(operation.Node.rich(cursor,
                operation.Rich.unwrap(r.start, after.text.asDelimited))),
                Some(a.copyContentMode(ret)))
            } else {
              val soc = rich.singleSpecials(r).map(_.range)
              val remaining = r.minusOrderedInside(soc)
              val range = (r.start, r.until + remaining.size * deli.wrapSizeOffset - 1)
              var fakePoints = if (visual.fix.start <= visual.move.start) {
                mode.Content.RichVisual(IntRange.len(range._1, 1), IntRange.endLen(range._2 + 1, deli.newDeliEndSize))
              } else {
                mode.Content.RichVisual(IntRange.endLen(range._2 + 1, deli.newDeliEndSize), IntRange.len(range._1, 1))
              }
              fakePoints = fakePoints.maybeEmpty(visual)
              DocTransaction(Seq(operation.Node.rich(cursor,
                operation.Rich.wrapNonOverlappingOrderedRanges(remaining, deli))),
                Some(a.copyContentMode(fakePoints)))
            }
        }
      } else if (a.isNodeVisual) {
        val v = a.asNodeVisual
        if (v.fix == v.move) {
          val node = a.node(v.fix)
          if (node.content.isRich) {
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

    override def available(a: DocState): Boolean = a.isRichVisual && super.available(a)

    override val description: String = s"wrap selection as ${deli.name}"
    override def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = a.asRichVisual match {
      case (cursor, rich, visual) =>
        val r = visual.merged
        val p = IntRange(r.start, r.until + deli.wrapSizeOffset)
        var fakeMode =
          if (deli.atomic) {
            mode.Content.RichVisual(p, p)
          } else if (visual.fix.start <= visual.move.start) {
            mode.Content.RichVisual(IntRange.len(p.start, 1), IntRange.endLen(p.until, deli.newDeliEndSize))
          } else {
            mode.Content.RichVisual(IntRange.endLen(p.until, deli.newDeliEndSize), IntRange.len(r.start, 1))
          }
        fakeMode = fakeMode.maybeEmpty(visual)
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
    override def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction =
      if (a.isRichVisual) {
        a.asRichVisual match {
          case (cursor, rich, visual) =>
            var r = visual.merged
            val g = rich.isSubRich(r)
            if (g.isDefined) r = g.get
            if (g.isDefined) {
              val p = IntRange(r.start, r.until + deli.wrapSizeOffset)
              var fakeMode =
                if (visual.fix.start <= visual.move.start) {
                  mode.Content.RichVisual(IntRange.len(p.start, 1), IntRange.endLen(p.until, deli.newDeliEndSize))
                } else {
                  mode.Content.RichVisual(IntRange.endLen(p.until, deli.newDeliEndSize), IntRange.len(r.start, 1))
                }
              fakeMode = fakeMode.maybeEmpty(visual)
              DocTransaction(Seq(
                operation.Node.rich(cursor, operation.Rich.wrap(r, deli))),
                Some(a.copyContentMode(fakeMode)))
            } else {
              DocTransaction.empty
            }
        }
      } else if (a.isNodeVisual) {
        val v = a.asNodeVisual
        if (v.fix == v.move) {
          val node = a.node(v.fix)
          if (node.content.isRich) {
            val rich = node.content.asInstanceOf[data.Content.Rich].content
            return DocTransaction(Seq(operation.Node.rich(v.fix, operation.Rich.wrap(IntRange(0, rich.size), deli))), None)
          }
        }
        DocTransaction.empty
      } else {
        throw new IllegalStateException("Not possible")
      }
  }).toMap
}
