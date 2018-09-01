package doc

import client.Client.ViewMessage
import model.{cursor, data, mode, operation, transaction}
import model.cursor.Node
import model.data.{Atom, Rich}
import model.mode.Content.CodeInside
import model.range.IntRange
import settings.Settings


case class DocState private (
  node: model.data.Node,
  zoom: cursor.Node,
  mode0: model.mode.Node,
  badMode: Boolean,
  userFoldedNodes: Map[String, Boolean]
) {
  def changeContentType(cur: cursor.Node,
    to: Option[data.Node.ContentType],
    opts: transaction.Node = Seq.empty
  ): DocTransaction = {
    val chidlren = if (!node(cur).has(data.Node.ChildrenType)) {
      to.flatMap(_.preferredChildrenType).map(a => operation.Node.AttributeChange(cur, data.Node.ChildrenType, Some(a))).toSeq
    } else {
      Seq.empty
    }
    DocTransaction(
      opts ++ Seq(operation.Node.AttributeChange(cur, data.Node.ContentType, to)) ++ chidlren, None)
  }

  def nodeRefRelative(uuid: String): String = {
    if (uuid == node.uuid) "" else model.data.Node.nodeRefRelative(uuid)
  }

  def goTo(cur: Node, settings: Settings, mustZoom: Boolean = false): DocTransaction = {
    val enableModal = settings.enableModal
    val noZoom =  !mustZoom && visible(cur)
    DocTransaction(Seq.empty,
      Some(model.mode.Node.Content(cur, node(cur).content.defaultMode(enableModal))),
      zoomAfter = if (noZoom) None else Some(cur), viewMessagesAfter = if (noZoom) Seq(ViewMessage.ScrollToNodeTop(cur)) else Seq.empty)
  }

  def focus: Node = mode0.focus


  def breakWhiteSpaceInserts: Boolean = mode.exists(_.breakWhiteSpaceInserts)


  def lookup(uuid: String) = node.lookup(uuid, cursor.Node.root)

  def quickSearch(tt: Seq[data.Unicode],
    isLaTeXMacro: Boolean,
    heading: Boolean,
    headingLevel: Int,
    code0: Boolean,
    deli: settings.SpecialKeySettings, viewport: Boolean): Seq[cursor.Node] = {
    val (n, cur) = if (viewport) (node(zoom), zoom) else (node, cursor.Node.root)
    val code = isLaTeXMacro || code0
    n.filter(cur, a => {
      (!isLaTeXMacro || a.isLaTeXMacro) &&
      (!heading || a.isHeading) &&
        (headingLevel <= 0 || a.heading.contains(headingLevel)) &&
        (
          a.content match {
            case data.Content.Code(_, _) => code
            case data.Content.Rich(j) =>
              !code && j.quickSearch(tt, deli)
          }
        )
    }).sortBy(cur => {
      val n = node(cur)
      (!n.isH1, !n.isHeading)
    })
  }


  def zoomId: String = node(zoom).uuid

  def mode: Option[model.mode.Node] = if (badMode) None else Some(mode0)


  def consistencyCheck(): Unit = {

    assert(node.get(zoom).isDefined, s"wrong zoom? $zoom")
    assert(mode0.inside(zoom), s"mode not inside zoom $mode0 $zoom")
    assert(visible(mode0.focus), s"mode hidden $mode0, $zoom")
    assert(visible(mode0.other), s"mode hidden $mode0, $zoom")
    if (isRich) {
      val (cur, rich, mo) = asRich
      def checkAtomicRichRange(a: IntRange) = {
        val r1 = rich.afters(a.start).next().range
        val r2 = rich.befores(a.until).next().range
        assert(r1 == r2 && a == r1)
      }
      def checkAtomicTextRichRange(a: IntRange) = {
        val r1 = rich.afters(a.start).next()
        val r2 = rich.befores(a.until).next()
        assert(r1.textRange == r2.textRange && a == r1.textRange)
      }
      def ret(mo: model.mode.Content.Rich): Unit = {
        mo match {
          case model.mode.Content.RichVisual(a, b) =>
            assert(a.nonEmpty)
            assert(b.nonEmpty)
            checkAtomicRichRange(a)
            checkAtomicRichRange(b)
          case model.mode.Content.RichInsert(i) =>
            assert(i <= rich.size)
            if (i != 0 && i != rich.size) {
              assert(rich.before(i).range.until == rich.after(i).range.start)
            }
          case model.mode.Content.RichNormal(i) =>
            checkAtomicRichRange(i)
          case model.mode.Content.RichCodeSubMode(r, _, o) =>
            checkAtomicTextRichRange(r)
            ret(o)
          case model.mode.Content.RichAttributeSubMode(r, o) =>
            checkAtomicTextRichRange(r)
            ret(o)
        }
      }
    }
  }

  def folded(a: cursor.Node): Boolean = {
    val no = node(a)
    userFoldedNodes.getOrElse(no.uuid, no.isH1)
  }

  /**
    * you need to ensure when you call this, there is no parenting folded nodes
    */
  def viewAsFolded(a: data.Node): Boolean = {
    a.uuid != zoomId && userFoldedNodes.getOrElse(a.uuid, a.isH1)
  }

  def viewAsNotFoldedAndNotHidden(a: cursor.Node): Boolean = inViewport(a) && !viewAsFolded(a) && !notVisible(a)

  def viewAsNotFolded(a: cursor.Node): Boolean = inViewport(a) && !viewAsFolded(a)

  /**
    * you need to ensure when you call this, there is no parenting folded nodes
    */
  def viewAsFolded(a: cursor.Node): Boolean = {
    assert(cursor.Node.contains(zoom, a))
    val no = node(a)
    a != zoom && userFoldedNodes.getOrElse(no.uuid, no.isH1)
  }

  /**
    * you need to ensure when you call this, there is no parenting folded nodes
    */
  def viewAsFolded(a: cursor.Node, default: Boolean): Boolean = {
    assert(cursor.Node.contains(zoom, a))
    val no = node(a)
    a != zoom && userFoldedNodes.getOrElse(no.uuid, default)
  }

  /**
    * reguardless if there is folded parents
    */
  def inViewport(a: cursor.Node): Boolean = cursor.Node.contains(zoom, a)



  def visibleParent(k: Node): cursor.Node = {
    var j = k
    while (j.size >= zoom.size) {
      if (visible(j)) return j
      j = cursor.Node.parent(j)
    }
    zoom
  }

  def visible(k: Node): Boolean = !notVisible(k)

  def notVisible(k: Node): Boolean = {
    if (!inViewport(k)) return true
    var n = k
    while (n.size > zoom.size) {
      n = cursor.Node.parent(n)
      if (viewAsFolded(n)) {
        return true
      }
    }
    false
  }

  def mover(): cursor.Node.Mover = new cursor.Node.Mover(node, zoom, viewAsFolded)


  def rich(n: cursor.Node): Rich = node(n).content.asInstanceOf[model.data.Content.Rich].content

  def isRich(n: cursor.Node): Boolean = node(n).content.isRich

  def isInsert: Boolean = mode match {
    case Some(model.mode.Node.Content(_, model.mode.Content.RichInsert(_))) => true
    case Some(model.mode.Node.Content(_, model.mode.Content.CodeInside(_, _))) => true
    case _ => false
  }

  def isInsertal: Boolean = mode match {
    case Some(model.mode.Node.Content(_, model.mode.Content.RichInsert(_))) => true
    case Some(model.mode.Node.Content(_, model.mode.Content.CodeInside(_, _))) => true
    case Some(model.mode.Node.Content(_, model.mode.Content.CodeNormal(b))) => b
    case _ => false
  }

  def isRichInsert: Boolean = mode match {
    case Some(model.mode.Node.Content(_, model.mode.Content.RichInsert(_))) => true
    case _ => false
  }


  def asRich: (cursor.Node, Rich, model.mode.Content.Rich) = mode match {
    case Some(model.mode.Node.Content(cur, rich: model.mode.Content.Rich)) => (cur, node(cur).rich, rich)
    case _ => throw new IllegalStateException("not supported")
  }

  def isNonEmptyRich: Boolean = mode match {
    case Some(model.mode.Node.Content(cur, rich: model.mode.Content.Rich)) => node(cur).content.nonEmpty
    case _ => false
  }

  def isRich: Boolean = mode match {
    case Some(model.mode.Node.Content(_, rich: model.mode.Content.Rich)) => true
    case _ => false
  }

  def isCode: Boolean = mode match {
    case Some(model.mode.Node.Content(_, rich: model.mode.Content.Code)) => true
    case _ => false
  }

  def isRichNormalOrVisual: Boolean = mode match {
    case Some(model.mode.Node.Content(_, model.mode.Content.RichVisual(_, _))) => true
    case Some(model.mode.Node.Content(_, model.mode.Content.RichNormal(a))) => true
    case _ => false
  }

  def isRichNormalOrInsert: Boolean = mode match {
    case Some(model.mode.Node.Content(_, model.mode.Content.RichInsert( _))) => true
    case Some(model.mode.Node.Content(_, model.mode.Content.RichNormal(a))) => true
    case _ => false
  }

  def isNonEmptyRichNormalOrVisual: Boolean = mode match {
    case Some(model.mode.Node.Content(_, model.mode.Content.RichVisual(_, _))) => true
    case Some(model.mode.Node.Content(_, model.mode.Content.RichNormal(a))) => a.nonEmpty
    case _ => false
  }


  def contentMode: model.mode.Content = mode match {
    case Some(c: model.mode.Node.Content)  => c.a
    case _ => throw new IllegalStateException("not supported")
  }

  def asContent: cursor.Node = mode match {
    case Some(model.mode.Node.Content(cur, _))  => cur
    case _ => throw new IllegalStateException("not supported")
  }

  def isContent: Boolean = mode match {
    case Some(model.mode.Node.Content(_, _))  => true
    case _ => false
  }

  def isNormal: Boolean = mode match {
    case Some(model.mode.Node.Content(_, a)) if a.isNormal => true
    case _ => false
  }

  def isCodeNormal: Boolean = mode match {
    case Some(model.mode.Node.Content(_, model.mode.Content.CodeNormal(_))) => true
    case _ => false
  }

  def isCodeInside: Boolean = mode match {
    case Some(model.mode.Node.Content(_, model.mode.Content.CodeInside(_, _))) => true
    case _ => false
  }

  def asRichNormalAtom: (cursor.Node, Rich, Atom) = {
    val (cur, rich, nv) = asRichNormal
    if (rich.isEmpty) throw new IllegalArgumentException("Wrong!")
    else {
      val t = rich.after(nv.focus.start)
      (cur, rich, t)
    }
  }

  def editCode(text: Atom, enableModal: Boolean): DocTransaction = {
    assert(text.text.isCodedAtomic)
    editCode(IntRange.len(text.textRange.start + 1, text.text.asDelimited.contentSize), enableModal)
  }

  def editCode(range: IntRange,
    enableModal: Boolean,
    modeBefore: model.mode.Content.Rich = mode.get.asInstanceOf[model.mode.Node.Content].a.asInstanceOf[model.mode.Content.Rich],
  ): DocTransaction = {
    DocTransaction(
      copyContentMode(model.mode.Content.RichCodeSubMode(range,
        CodeInside(if (!enableModal || modeBefore.isInstanceOf[model.mode.Content.RichInsert]) "insert" else "normal", 0),
        modeBefore)))
  }

  def editAttribute(range: IntRange,
    modeBefore: model.mode.Content.Rich = mode.get.asInstanceOf[model.mode.Node.Content].a.asInstanceOf[model.mode.Content.Rich]
  ): DocTransaction = {
    DocTransaction(
      copyContentMode(model.mode.Content.RichAttributeSubMode(range,
        modeBefore)))
  }

  def editAttribute(text: Atom): DocTransaction = {
    editAttribute(IntRange.len(text.textRange.start + 1, text.text.asDelimited.contentSize))
  }

  def asRichAtom: (cursor.Node, Rich, Atom)  = {
    isRich((c: cursor.Node, r: Rich, a: Atom) => return (c, r, a))
    throw new IllegalArgumentException("Not possible")
  }

  def isRich(a: (cursor.Node, Rich, Atom) => Boolean): Boolean = {
    mode.exists {
      case model.mode.Node.Content(cur, mode: model.mode.Content.Rich) =>
        val rich = node(cur).rich
        if (rich.isEmpty) {
          false
        } else {
          mode match {
            case model.mode.Content.RichInsert(i) =>
              i != 0 && i != rich.size && a(cur, rich, rich.after(i)) && a(cur, rich, rich.before(i))
            case _ =>
              val t = rich.after(mode.focus.start)
              a(cur, rich, t)
          }
        }
      case _ => false
    }
  }

  def isRichNormal(a: (Rich, Atom) => Boolean): Boolean = {
    if (isRichNormal) {
      val (_, rich, nv) = asRichNormal
      if (rich.isEmpty) false
      else {
        val t = rich.after(nv.focus.start)
        a(rich, t)
      }
    } else {
      false
    }
  }

  def isRichNormal: Boolean = mode match {
    case Some(model.mode.Node.Content(_, model.mode.Content.RichNormal(_))) => true
    case _ => false
  }

  def isRichVisual: Boolean = mode match {
    case Some(model.mode.Node.Content(_, model.mode.Content.RichVisual(_, _))) => true
    case _ => false
  }

  def isVisual: Boolean = mode match {
    case Some(model.mode.Node.Visual(_, _)) => true
    case Some(model.mode.Node.Content(_, model.mode.Content.RichVisual(_, _))) => true
    case _ => false
  }

  def isNodeVisual: Boolean = mode match {
    case Some(model.mode.Node.Visual(_, _)) => true
    case _ => false
  }

  def asNodeVisual: model.mode.Node.Visual = mode match {
    case Some(v@model.mode.Node.Visual(_, _)) => v
    case _ => throw new MatchError("Not possible")
  }

  def asCode: (cursor.Node, data.Content.Code) = {
    mode match {
      case Some(model.mode.Node.Content(n, c)) =>
        c match {
          case _: model.mode.Content.Code => (n, node(n).content.asInstanceOf[data.Content.Code])
          case _ => throw new IllegalArgumentException("Should not call this method with not applicable state")
        }

      case _ => throw new IllegalArgumentException("Should not call this method with not applicable state")
    }
  }

  def asCodeInside: cursor.Node = {
    mode match {
      case Some(model.mode.Node.Content(n, c)) =>
        c match {
          case t@model.mode.Content.CodeInside(_, _) => n
          case _ => throw new IllegalArgumentException("Should not call this method with not applicable state")
        }
      case _ => throw new IllegalArgumentException("Should not call this method with not applicable state")
    }
  }

  def asNormal: (cursor.Node, model.mode.Content.Normal) = {
    mode match {
      case Some(model.mode.Node.Content(n, c)) =>
        c match {
          case t@model.mode.Content.RichNormal(_) => (n, t)
          case t@model.mode.Content.CodeNormal(_) => (n, t)
          case _ => throw new IllegalArgumentException("Should not call this method with not applicable state")
        }
      case _ => throw new IllegalArgumentException("Should not call this method with not applicable state")
    }
  }

  def asRichVisual: (cursor.Node, Rich, model.mode.Content.RichVisual) = {
    mode match {
      case Some(o@model.mode.Node.Content(n, c)) =>
        val content = rich(n)
        c match {
          case v@model.mode.Content.RichVisual(fix, m) => (n, content, v)
          case _ => throw new IllegalArgumentException("Should not call this method with not applicable state")
        }
      case _ => throw new IllegalArgumentException("Should not call this method with not applicable state")
    }
  }

  def asRichNormalOrInsert: (cursor.Node, Rich, Int, Int) = {
    mode match {
      case Some(o@model.mode.Node.Content(n, c)) =>
        val content = rich(n)
        c match {
          case nor@model.mode.Content.RichNormal(r) => (n, content, r.start, r.until)
          case v@model.mode.Content.RichInsert(m) => (n, content, m, -1)
          case _ => throw new IllegalArgumentException("Should not call this method with not applicable state")
        }
      case _ => throw new IllegalArgumentException("Should not call this method with not applicable state")
    }
  }

  def asRichInsert: (cursor.Node, Rich, model.mode.Content.RichInsert) = {
    mode match {
      case Some(o@model.mode.Node.Content(n, c)) =>
        val content = rich(n)
        c match {
          case insert@model.mode.Content.RichInsert(r) => (n, content, insert)
          case _ => throw new IllegalArgumentException("Should not call this method with not applicable state")
        }
      case _ => throw new IllegalArgumentException("Should not call this method with not applicable state")
    }
  }


  def asRichNormal: (cursor.Node, Rich, model.mode.Content.RichNormal) = {
    mode match {
      case Some(o@model.mode.Node.Content(n, c)) =>
        val content = rich(n)
        c match {
          case nor@model.mode.Content.RichNormal(r) => (n, content, nor)
          case _ => throw new IllegalArgumentException("Should not call this method with not applicable state")
        }
      case _ => throw new IllegalArgumentException("Should not call this method with not applicable state")
    }
  }

  def copyContentMode(normal: model.mode.Content): model.mode.Node.Content = {
    mode match {
      case Some(o@model.mode.Node.Content(n, c)) =>
        o.copy(a = normal)
      case _ => throw new IllegalArgumentException("Should not call this method with not applicable state")
    }
  }

}
