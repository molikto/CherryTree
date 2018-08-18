package doc

import model.{cursor, data, mode}
import model.cursor.Node
import model.data.{Atom, Rich}


case class DocState(
  node: model.data.Node,
  zoom: cursor.Node,
  mode0: model.mode.Node,
  badMode: Boolean,
  userFoldedNodes: Map[String, Boolean]
) {

  def quickSearch(tt: Seq[data.Unicode],
    heading: Boolean,
    headingLevel: Int,
    code: Boolean,
    deli: settings.SpecialKeySettings, viewport: Boolean): Seq[cursor.Node] = {
    val (n, cur) = if (viewport) (node(zoom), zoom) else (node, cursor.Node.root)
    n.filter(cur, a => {
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

  assert(node.get(zoom).isDefined, s"wrong zoom? $zoom")
  assert(mode0.inside(zoom), s"mode not inside zoom $mode0 $zoom")

  def folded(a: cursor.Node): Boolean = {
    val no = node(a)
    userFoldedNodes.getOrElse(no.uuid, no.isH1)
  }

  def viewAsFolded(a: cursor.Node): Boolean = {
    assert(a.startsWith(zoom))
    val no = node(a)
    a != zoom && userFoldedNodes.getOrElse(no.uuid, no.isH1)
  }


  def hidden(k: Node): Boolean = {
    var n = k
    while (n.size > zoom.size) {
      n = cursor.Node.parent(n)
      if (viewAsFolded(n)) {
        return true
      }
    }
    false
  }

  def folded(a: cursor.Node, default: Boolean): Boolean = {
    assert(a.startsWith(zoom))
    val no = node(a)
    a != zoom && userFoldedNodes.getOrElse(no.uuid, default)
  }

  def mover(): cursor.Node.Mover = new cursor.Node.Mover(node, zoom, viewAsFolded)


  def rich(n: cursor.Node): Rich = node(n).content.asInstanceOf[model.data.Content.Rich].content

  def isRich(n: cursor.Node): Boolean = node(n).content.isRich

  def isInsert: Boolean = mode match {
    case Some(model.mode.Node.Content(_, model.mode.Content.RichInsert(_))) => true
    case Some(model.mode.Node.Content(_, model.mode.Content.CodeInside(_, _))) => true
    case _ => false
  }

  def isRichInsert: Boolean = mode match {
    case Some(model.mode.Node.Content(_, model.mode.Content.RichInsert(_))) => true
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


  def isNormal: Boolean = mode match {
    case Some(model.mode.Node.Content(_, a)) if a.isNormal => true
    case _ => false
  }

  def isCodeNormal: Boolean = mode match {
    case Some(model.mode.Node.Content(_, model.mode.Content.CodeNormal)) => true
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

  def isRich(a: (cursor.Node, Rich, Atom) => Boolean): Boolean = {
    if (isRichInsert) {
      val (cur, rich, nv) = asRichInsert
      if (rich.isEmpty) false
      else {
        val t = rich.after(nv.pos)
        a(cur, rich, t)
      }
    } else if (isRichNormalOrVisual) {
      val (cur, rich, nv) = asRichNormalOrVisual
      if (rich.isEmpty) false
      else {
        val t = rich.after(nv.focus.start)
        a(cur, rich, t)
      }
    } else {
      false
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

  def asCodeNormal : (cursor.Node, data.Content.Code) = {
    mode match {
      case Some(model.mode.Node.Content(n, c)) =>
        c match {
          case t@model.mode.Content.CodeNormal => (n, node(n).content.asInstanceOf[data.Content.Code])
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
          case t@model.mode.Content.CodeNormal => (n, t)
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

  def asRichNormalOrVisual: (cursor.Node, Rich, model.mode.Content.RichNormalOrVisual) = {
    mode match {
      case Some(o@model.mode.Node.Content(n, c)) =>
        val content = rich(n)
        c match {
          case nor@model.mode.Content.RichNormal(r) => (n, content, nor)
          case v@model.mode.Content.RichVisual(fix, m) => (n, content, v)
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
