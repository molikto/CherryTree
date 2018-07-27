package command.defaults

import client.Client
import command.{CommandCategory, CommandInterface}
import command.Key._
import doc.{DocState, DocTransaction}
import model.data.{Atom, SpecialChar}
import model.{mode, operation}
import model.range.IntRange

class RichVisual extends CommandCategory("text visual mode") {


  new Command {
    override val description: String = "enter text visual mode"
    override val defaultKeys: Seq[KeySeq] = Seq("v")
    override def available(a: DocState): Boolean = a.isNonEmptyRichNormalOrVisual
    override def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      val (_, rich, m) = a.asRichNormalOrVisual
      m match {
        case model.mode.Content.RichNormal(r) =>
          if (rich.isEmpty) {
            DocTransaction.empty
          } else {
            DocTransaction.mode(a.copyContentMode(model.mode.Content.RichVisual(r, r)))
          }
        case model.mode.Content.RichVisual(fix, move) =>
          DocTransaction.mode(a.copyContentMode(model.mode.Content.RichNormal(move)))
      }
    }

  }

  new Command {
    override val description: String = "swap movable and fixed cursor"
    override val defaultKeys: Seq[KeySeq] = Seq("o")
    override def available(a: DocState): Boolean = a.isRichVisual
    override def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = DocTransaction.mode(a.copyContentMode(a.asRichVisual._3.swap))
  }

  abstract class WrapCommand(deli: SpecialChar.Delimitation) extends DeliCommand(deli) {
    override def available(a: DocState): Boolean = a.isRichVisual
  }

  SpecialChar.nonCodedSplittable.map(deli => deli -> new WrapCommand(deli) {
    override val description: String = s"wrap selection in ${deli.name}"
    override def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = a.asRichVisual match {
      case (cursor, rich, visual) =>
        val r = visual.merged
        val soc = rich.singleSpecials(r).map(_.range)
        val remaining = r.minusOrderedInside(soc)
        val range = (r.start, r.until + remaining.size * deli.wrapSizeOffset - 1)
        val fakePoints = if (visual.fix.start <= visual.move.start) {
          mode.Content.RichVisual(IntRange(range._1), IntRange(range._2))
        } else {
          mode.Content.RichVisual(IntRange(range._2), IntRange(range._1))
        }
        DocTransaction(Seq(operation.Node.Content(cursor,
          operation.Content.Rich(operation.Rich.wrapNonOverlappingOrderedRanges(remaining, deli)))),
          Some(a.copyContentMode(fakePoints)))
    }
  }).toMap

  /**
    * code wrap CANNOT use surround!!! because this breaks cursor placement and might insert special char inside code
    * also we currently only wraps plain text
    */
  SpecialChar.codedNonEmpty.map(deli => deli -> new WrapCommand(deli) {
    override val description: String = s"wrap selection as ${deli.name}"
    override def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = a.asRichVisual match {
      case (cursor, rich, visual) =>
        val r = visual.merged
        val fakeMode =
          if (deli.atomic) {
            val p = IntRange(r.start, r.until + deli.wrapSizeOffset)
            mode.Content.RichVisual(p, p)
          } else if (visual.fix.start <= visual.move.start) {
            mode.Content.RichVisual(IntRange(r.start), IntRange(r.until + deli.wrapSizeOffset - 1))
          } else {
            mode.Content.RichVisual(IntRange(r.until + deli.wrapSizeOffset - 1), IntRange(r.start))
          }
        val ifs = rich.between(r)
        if (ifs.forall(_.isInstanceOf[Atom.PlainGrapheme])) {
          DocTransaction(Seq(
            operation.Node.Content(cursor,
              operation.Content.Rich(operation.Rich.wrapAsCoded(rich.subPlain(r), r, deli)))),
            Some(a.copyContentMode(fakeMode)))
        } else {
          DocTransaction.empty
        }
    }

  }).toMap

  SpecialChar.nonCodedNonSplittable.map(deli => deli -> new WrapCommand(deli) {
    override val description: String = s"wrap selection in ${deli.name}"
    override def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = a.asRichVisual match {
      case (cursor, rich, visual) =>
        var r = visual.merged
        val g = rich.isSubRich(r)
        if (g.isDefined) r = g.get
        if (g.isDefined) {
          val fakeMode =
            if (visual.fix.start <= visual.move.start) {
              mode.Content.RichVisual(IntRange(r.start), IntRange(r.until - 1 + deli.wrapSizeOffset))
            } else {
              mode.Content.RichVisual(IntRange(r.until - 1 + deli.wrapSizeOffset), IntRange(r.start))
            }
          DocTransaction(Seq(
            operation.Node.Content(cursor,
              operation.Content.Rich(operation.Rich.wrap(r, deli)))),
            Some(a.copyContentMode(fakeMode)))
        } else {
          DocTransaction.empty
        }
    }
  }).toMap
}
