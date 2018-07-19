package command.defaults

import client.Client
import command.CommandCollector
import command.Key._
import model.data.{InfoType, SpecialChar}
import model.{ClientState, mode, operation}
import model.range.IntRange

trait RichVisual extends CommandCollector {


  new Command {
    override val defaultKeys: Seq[KeySeq] = Seq("v")
    override def available(a: ClientState): Boolean = a.isRichNormalOrVisual
    override def action(a: ClientState, count: Int): Client.Update = {
      val (_, rich, m) = a.asRichNormalOrVisual
      m match {
        case model.mode.Content.RichNormal(r) =>
          if (rich.isEmpty) {
            Client.Update.empty
          } else {
            Client.Update.mode(a.copyContentMode(model.mode.Content.RichVisual(r, r)))
          }
        case model.mode.Content.RichVisual(fix, move) =>
          Client.Update.mode(a.copyContentMode(model.mode.Content.RichNormal(move)))
      }
    }
  }

  new Command {
    override val defaultKeys: Seq[KeySeq] = Seq("o")
    override def available(a: ClientState): Boolean = a.isRichVisual
    override def action(a: ClientState, count: Int): Client.Update = Client.Update.mode(a.copyContentMode(a.asRichVisual._3.swap))
  }

  abstract class WrapCommand(deli: SpecialChar.Delimitation) extends DeliCommand(deli) {
    override def available(a: ClientState): Boolean = a.isRichVisual
  }

  val formatLikeWraps: Map[SpecialChar.Delimitation, Command] = SpecialChar.formatLike.map(deli => deli -> new WrapCommand(deli) {
    override def action(a: ClientState, count: Int): Client.Update = a.asRichVisual match {
      case (cursor, rich, visual) =>
        val r = visual.merged
        val (_, soc, _) = rich.infoAndSingleSpecials(r)
        val remaining = r.minusOrderedInside(soc)
        val range = (r.start, r.until + remaining.size * deli.wrapSizeOffset - 1)
        val fakePoints = if (visual.fix.start <= visual.move.start) {
          mode.Content.RichVisual(IntRange(range._1), IntRange(range._2))
        } else {
          mode.Content.RichVisual(IntRange(range._2), IntRange(range._1))
        }
        Client.Update(Seq(operation.Node.Content(cursor,
          operation.Content.Rich(operation.Rich.wrapNonOverlappingOrderedRanges(remaining, deli)))),
          Some(a.copyContentMode(fakePoints)))
    }
  }).toMap

  /**
    * code wrap CANNOT use surround!!! because this breaks cursor placement and might insert special char inside code
    * also we currently only wraps plain text
    */
  val codedWraps: Map[SpecialChar.Delimitation, Command] = SpecialChar.coded.map(deli => deli -> new WrapCommand(deli) {
    override def action(a: ClientState, count: Int): Client.Update = a.asRichVisual match {
      case (cursor, rich, visual) =>
        val r = visual.merged
        val fakeMode =
          if (deli.isAtomic) {
            val p = IntRange(r.start, r.until + deli.wrapSizeOffset)
            mode.Content.RichVisual(p, p)
          } else if (visual.fix.start <= visual.move.start) {
            mode.Content.RichVisual(IntRange(r.start), IntRange(r.until + deli.wrapSizeOffset - 1))
          } else {
            mode.Content.RichVisual(IntRange(r.until + deli.wrapSizeOffset - 1), IntRange(r.start))
          }
        val ifs = rich.info(r)
        if (ifs.forall(_.ty == InfoType.Plain)) {
          Client.Update(Seq(
            operation.Node.Content(cursor,
              operation.Content.Rich(operation.Rich.wrapAsCoded(rich.subPlain(r), r, deli)))),
            Some(a.copyContentMode(fakeMode)))
        } else {
          Client.Update.empty
        }
    }

  }).toMap

  val linkLikeWraps: Map[SpecialChar.Delimitation, Command] = SpecialChar.linkLike.map(deli => deli -> new WrapCommand(deli) {
    override def action(a: ClientState, count: Int): Client.Update = a.asRichVisual match {
      case (cursor, rich, visual) =>
        val r = visual.merged
        if (rich.isSubRich(r)) {
          val fakeMode =
            if (visual.fix.start <= visual.move.start) {
              mode.Content.RichVisual(IntRange(r.start), IntRange(r.until - 1 + deli.wrapSizeOffset))
            } else {
              mode.Content.RichVisual(IntRange(r.until - 1 + deli.wrapSizeOffset), IntRange(r.start))
            }
          Client.Update(Seq(
            operation.Node.Content(cursor,
              operation.Content.Rich(operation.Rich.wrap(r, deli)))),
            Some(a.copyContentMode(fakeMode)))
        } else {
          Client.Update.empty
        }
    }
  }).toMap
}
