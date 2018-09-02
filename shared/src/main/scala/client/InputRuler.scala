package client

import java.util.regex.Pattern

import doc.{DocState, DocTransaction}
import model.cursor.Node
import model.data.{SpecialChar, Unicode}
import model.{data, operation}


abstract class InputRule(val a: String) {
  val pt = Pattern.compile(a + "$")
  def create(doc: DocState, at: model.cursor.Node, pos: Int, start: Int, end: Int): DocTransaction
}

 class ReplaceInputRule(a: String, val b: String) extends InputRule(a) {
   val rep = model.data.Unicode(b)
   override def create(doc: DocState, at: model.cursor.Node, pos: Int, start: Int, end: Int): DocTransaction = {
     DocTransaction(
       Seq(operation.Node.rich(at, operation.Rich.replacePlain(start, end, rep))),
       None
     )
   }
 }

class ContentTypeRule(a: String, ty: data.Node.ContentType) extends InputRule(Pattern.quote(a)) {
  override def create(doc: DocState, at: Node, pos: Int, start: Int, end: Int): DocTransaction = {
    if (pos == a.length) {
      doc.changeContentType(at, Some(ty), Seq(operation.Node.rich(at, operation.Rich.delete(start, end))))
    } else {
      DocTransaction.empty
    }
  }
}


class ParentChildrenTypeRule(a: String, ty: data.Node.ChildrenType) extends InputRule(Pattern.quote(a)) {
  override def create(doc: DocState, at: Node, pos: Int, start: Int, end: Int): DocTransaction = {
    if (pos == a.length && at != doc.zoom) {
      val par = model.cursor.Node.parent(at)
      if (doc.node(par).attribute(data.Node.ChildrenType).getOrElse(data.Node.ChildrenType.UnorderedList) != ty) {
        return DocTransaction(
          Seq(
            operation.Node.rich(at, operation.Rich.delete(start, end)),
            operation.Node.AttributeChange(par, data.Node.ChildrenType, Some(ty))),
          None)
      }
    }
    DocTransaction.empty
  }
}

trait InputRuler { self: Client =>

  private val inputRules = Seq(
    new ReplaceInputRule("--", "–"),
    new ReplaceInputRule("–-", "—"),
    new ReplaceInputRule("-–", "—"),
    new ReplaceInputRule("""\(C\)""", "©"),
    new ReplaceInputRule("""\(R\)""", "®"),
    new ReplaceInputRule("""\(TM\)""", "™"),
    new ReplaceInputRule("""\.\.\.""", "…"),
    new ReplaceInputRule("""(?:^|[\s\{\[\(\<'"\u2018\u201C])(")""", "“"), // order is important!!
    new ReplaceInputRule("\"", "”"),
    new ReplaceInputRule("""(?:^|[\s\{\[\(\<'"\u2018\u201C])(')""", "‘"),
    new ReplaceInputRule("""(?:[\s])(['‘])(?:tis\b|twas\b)""", "’"),
    new ReplaceInputRule("""(?:\s)(['‘])(?:[0-9]+s*\b)""", "’"),
    new ReplaceInputRule("'", "’"),
    new ReplaceInputRule("<->", "↔"), new ReplaceInputRule("←>", "↔"),
    new ReplaceInputRule("->", "→"),
    new ReplaceInputRule("<-", "←"),
    new ReplaceInputRule("-->", "⟶"), new ReplaceInputRule(Pattern.quote("–>"), "⟶"),
    new ReplaceInputRule("<--", "⟵"), new ReplaceInputRule("←-", "⟵"),
    new ReplaceInputRule("<==>", "⇔"), new ReplaceInputRule("⇐>", "⇔"),
    new ReplaceInputRule("<==", "⇐"),
    new ReplaceInputRule("==>", "⇒"),
    new ContentTypeRule("> ", data.Node.ContentType.Cite),
    new ContentTypeRule("###### ", data.Node.ContentType.Heading(6)), // shit! order is important!
    new ContentTypeRule("##### ", data.Node.ContentType.Heading(5)),
    new ContentTypeRule("#### ", data.Node.ContentType.Heading(4)),
    new ContentTypeRule("### ", data.Node.ContentType.Heading(3)),
    new ContentTypeRule("## ", data.Node.ContentType.Heading(2)),
    new ContentTypeRule("# ", data.Node.ContentType.Heading(1)),
    new ParentChildrenTypeRule("1. ", data.Node.ChildrenType.OrderedList),
    new ParentChildrenTypeRule("- ", data.Node.ChildrenType.DashList),
    new ParentChildrenTypeRule("* ", data.Node.ChildrenType.UnorderedList),
  )

  def extraInputRuleOperation(d: DocState, op: model.transaction.Node): Option[DocTransaction] = {
    op match {
      case Seq(operation.Node.Content(at, operation.Content.Rich(a))) =>
        a.canBeSmartInsert(d.node(at).rich) match {
          case Some((before, pos, ins)) =>
            inputRules.foreach(i => {
              val mm = i.pt.matcher(before.str + ins.str)
              if (mm.find()) {
                val g = mm.groupCount()
                val start = mm.start(g) - before.size + pos
                val end = mm.end(g) - before.size + pos
                val a = i.create(d, at, pos + ins.size, start, end)
                return if (a == DocTransaction.empty) None else Some(a)
              }
            })
          case None => None
        }
      case _ => None
    }
    None
  }
}
