package client

import java.util.regex.Pattern

import doc.{DocState, DocTransaction}
import model.cursor.Node
import model.data.SpecialChar
import model.operation


abstract class InputRule(val a: String) {
  val pt = Pattern.compile(a)
  def create(at: model.cursor.Node, pos: Int, start: Int, end: Int): DocTransaction
}

 class ReplaceInputRule(a: String, val b: String) extends InputRule(a) {
   val rep = model.data.Unicode(b)
   override def create(at: model.cursor.Node, pos: Int, start: Int, end: Int): DocTransaction = {
     DocTransaction(
       Seq(operation.Node.rich(at, operation.Rich.replacePlain(start, end, rep))),
       None
     )
   }
 }

trait InputRuler { self: Client =>

  private val inputRules = Seq(
    new ReplaceInputRule("--$", "–"),
    new ReplaceInputRule("–-$", "—"),
    new ReplaceInputRule("-–$", "—"),
    new ReplaceInputRule("""\(C\)$""", "©"),
    new ReplaceInputRule("""\(R\)$""", "®"),
    new ReplaceInputRule("""\(TM\)$""", "™"),
    new ReplaceInputRule("""\.\.\.$""", "…"),
    new ReplaceInputRule("""(?:^|[\s\{\[\(\<'"\u2018\u201C])(")$""", "“"),
    new ReplaceInputRule("\"$", "”"),
    new ReplaceInputRule("""(?:^|[\s\{\[\(\<'"\u2018\u201C])(')$""", "‘"),
    new ReplaceInputRule("""(?:[\s])(['‘])(?:tis\b|twas\b)$""", "’"),
    new ReplaceInputRule("""(?:\s)(['‘])(?:[0-9]+s*\b)$""", "’"),
    new ReplaceInputRule("'$", "’"),
  )

  def extraInputRuleOperation(d: DocState, op: model.transaction.Node): Option[DocTransaction] = {
    op match {
      case Seq(operation.Node.Content(at, operation.Content.Rich(a)))  =>
        a.canBeSmartInsert(d.node(at).rich) match {
          case Some((before, pos, ins)) =>
            inputRules.foreach(i => {
              val mm = i.pt.matcher(before.str + ins.str)
              if (mm.find()) {
                val g = mm.groupCount()
                val start = mm.start(g) - before.size + pos
                val end = mm.end(g) - before.size + pos
                return Some(i.create(at, pos, start, end))
              }
            })
          case None => None
        }
      case _ => None
    }
    None
  }
}
