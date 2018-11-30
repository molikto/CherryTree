package doc

import java.util.UUID

import client.Client.ViewMessage
import doc.DocState.NodeAndType
import model.{cursor, data, mode, operation, transaction}
import model.cursor.Node
import model.data.NodeType
import model.data.{Atom, Rich}
import model.mode.Content.CodeInside
import model.range.IntRange
import search.{Search, SearchOccurrence}
import settings.Settings

object DocState {
  private case class SearchCache(search: Search, mode: model.mode.Node, enableModal: Boolean) {
    var res: Seq[SearchOccurrence] = null
  }
  case class NodeAndType(val node: model.data.Node, folderType: NodeType, nodeType: NodeType) {
    def isList = nodeType.isList(folderType)
  }
}

case class DocState private (
  node: model.data.Node,
  zoom: cursor.Node,
  mode0: model.mode.Node,
  badMode: Boolean,
  userFoldedNodes: Map[UUID, Boolean]
) {

  val rootNodeType = node.attribute(NodeType)
      .filter(NodeType.folders.contains).getOrElse(NodeType.folders.head)

  def nodeAndType(a: model.cursor.Node): NodeAndType = {
    var t = rootNodeType
    var folderType = t
    var depth = 0
    var n = node
    while (depth < a.length) {
      n = n.childs(a(depth))
      t = n.attribute(NodeType).getOrElse(t.defaultChildrenType(folderType))
      if (t.isFolder) folderType = t
      depth += 1
    }
    NodeAndType(n, folderType, t)
  }

  def nodeType(a: model.cursor.Node) = {
    var t = rootNodeType
    var folderType = t
    var depth = 0
    var n = node
    while (depth < a.length) {
      n = n.childs(a(depth))
      t = n.attribute(NodeType).getOrElse(t.defaultChildrenType(folderType))
      if (t.isFolder) folderType = t
      depth += 1
    }
    t
  }

  def allowedChildrenType(a: model.cursor.Node) = {
    var t = rootNodeType
    var j = rootNodeType.allowedChildrenType(t)
    var folderType = t
    var depth = 0
    var n = node
    while (depth < a.length) {
      n = n.childs(a(depth))
      t = n.attribute(NodeType).getOrElse(t.defaultChildrenType(folderType))
      if (t.isFolder) folderType = t
      j = t.allowedChildrenType(folderType)
      depth += 1
    }
    j
  }

  def defaultChildrenType(a: model.cursor.Node) = {
    var t = rootNodeType
    var j = rootNodeType.defaultChildrenType(t)
    var folderType = t
    var depth = 0
    var n = node
    while (depth < a.length) {
      n = n.childs(a(depth))
      t = n.attribute(NodeType).getOrElse(j)
      if (t.isFolder) folderType = t
      j = t.defaultChildrenType(folderType)
      depth += 1
    }
    j
  }


  def canBe(cur: model.cursor.Node, c: NodeType): Boolean = {
    if (cur == model.cursor.Node.root) {
      c.isFolder
    } else {
      allowedChildrenType(model.cursor.Node.parent(cur)).contains(c)
    }
  }

  def canChangeTo(a: Node, c: NodeType): Boolean = {
    val cur = nodeType(a)
    // for nodes cannot have content, you cannot change to them, you can just insert them
    // also you cannot change back from them, because you have undo...
    if (cur == c) false else canBe(a, c)
  }

  def changeNodeTypeHeadingLevel(cur: cursor.Node,
                                 to: data.NodeType,
                                 opts: transaction.Node = Seq.empty
  ): DocTransaction = {
    val tt = if (cur == model.cursor.Node.root) Some(to) else if (defaultChildrenType(model.cursor.Node.parent(cur)) == to) None else Some(to)
    val nc = node(cur)
    val headingOp = if (tt.nonEmpty && nc.heading.isEmpty) { // we better find what's the parent heading level
      if (to.isHeading == 1) {
        val parentNode = node(model.cursor.Node.parent(cur))
        val toHeading = parentNode.heading.map(_ + 1)
            .orElse(parentNode.childs.flatMap(_.heading).headOption)
        toHeading match {
          case Some(i) =>
            Seq(operation.Node.AttributeChange(cur, data.Node.HeadingLevel, toHeading))
          case None =>
            Seq.empty
        }
      } else if (to.isHeading == 2) {
        Seq(operation.Node.AttributeChange(cur, data.Node.HeadingLevel, Some(1)))
      } else {
        Seq.empty
      }
    } else {
      Seq.empty
    }
    DocTransaction(
      opts ++ Seq(operation.Node.AttributeChange(cur, data.NodeType, tt)) ++ headingOp, None)
  }

  def nodeRefRelative(uuid: UUID, zoomToEmpty: Boolean = true): String = {
    if (uuid == node.uuid && zoomToEmpty) "" else model.data.Node.nodeRefRelative(uuid)
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


  def lookup(uuid: UUID) = node.lookup(uuid, cursor.Node.root)
  def find[T](pred: model.data.Node => Option[T]) = node.find(pred, cursor.Node.root)


  private var lastSearch: DocState.SearchCache = null
  /**
    * we only return the first one now...
    */
  def searchInShown(a: Search, enableModal: Boolean): Seq[SearchOccurrence] = {
    val cacheKey = DocState.SearchCache(a, mode0, enableModal)
    if (cacheKey != lastSearch) {
      def inner(): Seq[SearchOccurrence] = {
        val mv = mover()
        // this is the inclusive position the atom of the match start should be with in
        val searchBefore = a.direction == -1
        val (startNode, startPos) = mode0 match {
          case model.mode.Node.Content(node, a) =>
            def rec(a: model.mode.Content): Int = {
              a match {
                case n: model.mode.Content.RichNormalOrVisual =>
                  if (searchBefore) n.focus.start - 1 else n.focus.until
                case i: model.mode.Content.RichInsert =>
                  i.pos
                case s: model.mode.Content.RichSelection =>
                  if (searchBefore) s.start - 1 else s.end
                case a: model.mode.Content.RichSubMode =>
                  rec(a.modeBefore)
                case c: model.mode.Content.Code =>
                  0
              }
            }
            (node, rec(a))
          case model.mode.Node.Visual(fix, to) =>
            if (enableModal) {
              (to, if (searchBefore) 0 else Int.MaxValue)
            } else {
              (to, 0)
            }
        }
        // selection/normal/visual mode, search after exclude range, before include range
        // line mode, after include, before exclude
        var res: Option[IntRange] = None
        def rep(startNode: cursor.Node, startPos: Int, isRev: Boolean): Seq[SearchOccurrence] = {
          var n = startNode
          var pos = startPos
          while (n != null) {
            res = node(n).content.search(a, pos)
            if (res.isDefined) {
              return Seq(SearchOccurrence(n, res.get))
            } else {
              if (a.direction == -1) {
                n = mv.visualUp(n).orNull
                pos = Int.MaxValue
              } else {
                n = mv.visualDown(n).orNull
                pos = 0
              }
            }
          }
          if (!isRev) {
            if (searchBefore) {
              rep(mv.visualBottom(zoom), Int.MaxValue, true)
            } else {
              rep(zoom, 0, true)
            }
          } else {
            Seq.empty
          }
        }
        rep(startNode, startPos, false)
      }
      cacheKey.res = inner()
      lastSearch = cacheKey
    }
    lastSearch.res
  }

  def quickSearch(tt: Set[data.Unicode],
    hashes: Set[data.Unicode],
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
              !code && j.quickSearch(tt, hashes, deli)
          }
        )
    },
      // not current search root, and not ignored in search
      a => a.uuid != n.uuid && a.ignoreInSearch).sortBy(cur => {
      val n = node(cur)
      (!n.isFolder, !n.isHeading)
    })
  }


  def zoomId: UUID = node(zoom).uuid

  def mode: Option[model.mode.Node] = if (badMode) None else Some(mode0)


  def consistencyCheck(enableModal: Boolean): Unit = {
    if (!model.debug_model) return

    assert(node.get(zoom).isDefined, s"wrong zoom? $zoom")
    assert(mode0.inside(zoom), s"mode not inside zoom $mode0 $zoom")

    if (visible(mode0.focus)) {
    } else {
      assert(false)
    }
    if (visible(mode0.other)) {
    } else {
      assert(false)
    }
    if (isRich) {
      val (cur, rich, mo) = asRich
      if (enableModal) {
        assert(!mo.isInstanceOf[model.mode.Content.RichSelection])
      } else {
        assert(!mo.isInstanceOf[model.mode.Content.RichNormalOrVisual])
      }
      def checkAtomicRichRange(a: IntRange) = {
        val r1 = rich.afters(a.start).next().range
        val r2 = rich.befores(a.until).next().range
        if (r1 == r2 && a == r1) {
        } else {
          val a = 1
          assert(false, "what is this?")
        }
      }
      def checkAtomicTextRichRange(a: IntRange) = {
        val r1 = rich.afters(a.start).next()
        val r2 = rich.befores(a.until).next()
        if (r1.textRange == r2.textRange && a == r1.textRange) {
        } else {
          assert(false)
        }
      }
      def ret(mo: model.mode.Content.Rich): Unit = {
        mo match {
          case v@model.mode.Content.RichSelection(a, b) =>
            //checkAtomicTextRichRange(v.merged)
          case model.mode.Content.RichVisual(a, b) =>
            assert(b.nonEmpty)
            checkAtomicRichRange(a)
            checkAtomicRichRange(b)
          case model.mode.Content.RichInsert(i) =>
            assert(i <= rich.size)
            if (i != 0 && i != rich.size) {
              assert(rich.before(i).range.until == rich.after(i).range.start)
            }
          case model.mode.Content.RichNormal(i) =>
            if (rich.isEmpty) {
              assert(i == IntRange(0, 0))
            } else {
              checkAtomicRichRange(i)
            }
          case model.mode.Content.RichCodeSubMode(r, _, o) =>
            //checkAtomicTextRichRange(r)
            ret(o)
          case model.mode.Content.RichAttributeSubMode(r, o) =>
            //checkAtomicTextRichRange(r)
            ret(o)
        }
      }
      ret(mo)
    }
  }

  def folded(a: cursor.Node): Boolean = {
    val no = node(a)
    userFoldedNodes.getOrElse(no.uuid, no.isFolder)
  }

  /**
    * you need to ensure when you call this, there is no parenting folded nodes
    */
  def viewAsFolded(a: data.Node): Boolean = {
    a.uuid != zoomId && userFoldedNodes.getOrElse(a.uuid, a.isFolder)
  }

  def viewAsNotFoldedAndNotHidden(a: cursor.Node): Boolean = inViewport(a) && !viewAsFolded(a) && !notVisible(a)

  def viewAsNotFolded(a: cursor.Node): Boolean = inViewport(a) && !viewAsFolded(a)

  /**
    * you need to ensure when you call this, there is no parenting folded nodes
    */
  def viewAsFolded(a: cursor.Node): Boolean = {
    assert(cursor.Node.contains(zoom, a))
    val no = node(a)
    a != zoom && userFoldedNodes.getOrElse(no.uuid, no.isFolder)
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

  def isRichNonSub: Boolean = mode match {
    case Some(model.mode.Node.Content(_, rich: model.mode.Content.RichSubMode)) => false
    case Some(model.mode.Node.Content(_, rich: model.mode.Content.Rich)) => true
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

  def isRichNormalOrNoneEmptyVisual: Boolean = mode match {
    case Some(model.mode.Node.Content(_, model.mode.Content.RichVisual(a, _))) => a.nonEmpty
    case Some(model.mode.Node.Content(_, model.mode.Content.RichNormal(a))) => true
    case _ => false
  }

  def isRichNormalOrInsert: Boolean = mode match {
    case Some(model.mode.Node.Content(_, model.mode.Content.RichInsert( _))) => true
    case Some(model.mode.Node.Content(_, model.mode.Content.RichNormal(a))) => true
    case _ => false
  }


  def contentMode: model.mode.Content = mode match {
    case Some(c: model.mode.Node.Content)  => c.a
    case _ => throw new IllegalStateException("not supported")
  }

  def asSingle: cursor.Node = mode match {
    case Some(model.mode.Node.Content(cur, _))  => cur
    case Some(model.mode.Node.Visual(fix, move)) if fix == move => fix
    case _ => throw new IllegalStateException("not supported")
  }

  def isSingle: Boolean = mode match {
    case Some(model.mode.Node.Content(_, _))  => true
    case Some(model.mode.Node.Visual(fix, move)) if fix == move => true
    case _ => false
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
    editAttribute(IntRange(text.textRange.start + 1 + text.text.asDelimited.contentSize, text.textRange.until - 1))
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

  def isRichRange: Boolean = mode match {
    case Some(model.mode.Node.Content(_, r: model.mode.Content.RichRange)) => true
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
    case _ => throw new IllegalArgumentException("not possible")
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

  def asRichRange: (cursor.Node, Rich, model.mode.Content.RichRange) = {
    mode match {
      case Some(o@model.mode.Node.Content(n, c)) =>
        val content = rich(n)
        c match {
          case v: model.mode.Content.RichRange => (n, content, v)
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

  def asRichNormalOrVisual: (cursor.Node, Rich, model.mode.Content.RichNormalOrVisual) = {
    mode match {
      case Some(o@model.mode.Node.Content(n, c)) =>
        val content = rich(n)
        c match {
          case nor: model.mode.Content.RichNormalOrVisual => (n, content, nor)
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
