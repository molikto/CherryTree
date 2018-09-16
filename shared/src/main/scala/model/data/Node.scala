package model.data

import java.util.UUID

import doc.DocTransaction
import model._
import Node.{ChildrenType, ContentType}
import model.range.IntRange
import scalatags.Text.all._
import search.{Search, SearchOccurrence}

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

trait NodeTag[T] {
  private[model] val name: String
  private[model] def parse(a: String): T
  private[model] def serialize(t: T): String
}

// LATER simple type of node, so that it can be article, ordered list, unordered list, quote
case class Node(
  uuid: String,
  content: Content,
  attributes: Map[String, String],
  childs: Seq[Node]) {


  def assertNewNodes(strings: Seq[String]): Unit = {
    if (strings.contains(uuid)) {
      throw new IllegalStateException("Not allowed")
    }
    childs.foreach(_.assertNewNodes(strings))
  }

  def isLaTeXMacro: Boolean =
    content match {
      case c: Content.Code if c.ty == LaTeXMacro =>
        true
      case _ =>
        false
    }

  lazy val selfTags = content match {
    case Content.Rich(rich) => rich.tags
    case _ => Map.empty
  }


  def macros: Seq[String] = {
    if (macros_ == null) {
      var cur: String = null
      content match {
        case c: Content.Code if c.ty == LaTeXMacro =>
          cur = c.unicode.str
        case _ =>
      }
      var res: Seq[String] = if (cur == null) null else Seq(cur)
      for (c <- childs) {
        val cm = c.macros
        if (cm.nonEmpty) {
          if (res == null) {
            res = cm
          } else {
            res = res ++ cm
          }
        }
      }
      macros_ = if (res == null) Seq.empty else res
    }
    macros_
  }

  private var macros_ : Seq[String] = null

  def refOfThis() = Node.nodRef( uuid)

  def count: Int = 1 + childs.map(_.count).sum
  def size: Int = content.size + childs.map(_.size).sum

  def lookup(_2: String, currentCur: model.cursor.Node = model.cursor.Node.root): Option[model.cursor.Node] = {
    if (uuid == _2) {
      Some(currentCur)
    } else {
      childs.zipWithIndex.foreach(a =>  {
        val res = a._1.lookup(_2, currentCur :+ a._2)
        if (res.isDefined) {
          return res
        }
      })
      None
    }
  }


  private def filter0(cur: model.cursor.Node, a: Node => Boolean, bf: ArrayBuffer[model.cursor.Node]): Unit = {
    if (a(this)) bf.append(cur)
    childs.zipWithIndex.foreach(pair => {
      pair._1.filter0(cur :+ pair._2, a, bf)
    })
  }

  def filter(cur: model.cursor.Node, a: Node => Boolean): Seq[model.cursor.Node] = {
    val bf = new ArrayBuffer[model.cursor.Node]()
    filter0(cur, a, bf)
    bf
  }


  def lastDefined(a: model.cursor.Node): model.cursor.Node = {
    var len = a.length
    while (len >= 0) {
      val can = a.take(len)
      if (get(can).isDefined) {
        return can
      }
      len -= 1
    }
    model.cursor.Node.root
  }

  def isH1: Boolean = attribute(ContentType).contains(ContentType.Heading(1))
  def isHeading: Boolean = attribute(ContentType).exists(_.isInstanceOf[ContentType.Heading])

  def heading: Option[Int] = attribute(ContentType).filter(_.isInstanceOf[ContentType.Heading]).map(_.asInstanceOf[ContentType.Heading].i)


  def foreachNode(a: Node => Unit): Unit = {
    a(this)
    childs.foreach(_.foreachNode(a))
  }

  def foreach(a: (model.cursor.Node, Node) => Unit, cur: model.cursor.Node): Unit = {
    a(cur, this)
    childs.zipWithIndex.foreach(p => p._1.foreach(a, cur :+ p._2))
  }

  def allChildrenUuids(cur: model.cursor.Node, in: Map[String, Boolean]): Seq[model.cursor.Node] = {
    childs.zipWithIndex.flatMap(c => {
      val cs = c._1.allChildrenUuids(cur :+ c._2, in)
      if (in.get(c._1.uuid).contains(true)) cs :+ (cur :+ c._2) else cs
    })
  }

  def cloneNode(): Node = copy(uuid = UUID.randomUUID().toString, childs = Node.cloneNodes(childs))


  def has[T](t: NodeTag[T]): Boolean = attributes.get(t.name).exists(_.nonEmpty)
  def has(t: String): Boolean = attributes.get(t).exists(_.nonEmpty)

  def clear[T](a: NodeTag[T]) : Node = copy(attributes = attributes - a.name)
  def clear(a: String) : Node = copy(attributes = attributes - a)


  def attribute[T](t: NodeTag[T], a: Option[T]): Node = a match {
    case Some(a) => attribute(t, a)
    case None => clear(t)
  }

  def attribute[T](t: NodeTag[T], a: T): Node = copy(attributes = attributes.updated(t.name, t.serialize(a)))
  def attribute(t: String, a: String): Node = copy(attributes = attributes.updated(t, a))

  def attribute[T](a: NodeTag[T]): Option[T] = attributes.get(a.name).filter(_.nonEmpty).map(a.parse)
  def attribute(a: String): String = attributes.getOrElse(a, "")

  def contentType: Option[ContentType] = attribute(ContentType)


  def rich : Rich = content.asInstanceOf[Content.Rich].content


  def map(c: model.cursor.Node, transform: Node => Node): Node = {
    if (c.isEmpty) {
      transform(this)
    } else {
      copy(childs = childs.patch(c.head, Seq(childs(c.head).map(c.tail, transform)), 1))
    }
  }

  def get(a: model.cursor.Node): Option[Node] =
    if (a.isEmpty) Some(this) else if (a.head >= childs.size) None else childs(a.head).get(a.tail)

  def apply(c: model.cursor.Node): Node = if (c.isEmpty) this else childs(c.head)(c.tail)

  def apply(r: range.Node): Seq[Node] = this(r.parent)(r.childs)

  def apply(r: IntRange): Seq[Node] = childs.slice(r.start, r.until)

  def delete(d: range.Node): Node = map(d.parent, _.delete(d.childs))

  private def delete(r: IntRange): Node =
    copy(childs = childs.patch(r.start, Seq.empty, r.size))

  private def insert(i: Int, cs: Seq[Node]): Node = {
    if (i < 0 || i > childs.size) throw new IllegalArgumentException("Insertion is out of bound")
    copy(childs = childs.patch(i, cs, 0))
  }

  def insert(c: model.cursor.Node, cs: Seq[Node]): Node =
    map(model.cursor.Node.parent(c), a => a.insert(c.last, cs))

  def move(r: range.Node, at: model.cursor.Node): Node = {
    val a = this(r)
    delete(r).insert(r.transformAfterDeleted(at).get, a)
  }


  def toScalaTags(hasWrapper: Boolean): Frag = {
    def contentWithoutP = content match {
      case Content.Rich(t) => Text.toScalaTags(t.text)
      case c@Content.Code(u, lang) =>
        if (c.ty == Embedded.HTML) raw(u.str)
        else pre(u.str)
    }
    def children: Frag = attribute(ChildrenType) match {
      case Some(ChildrenType.Paragraphs) =>
        childs.map(_.toScalaTags)
      case Some(ChildrenType.OrderedList) =>
        if (childs.isEmpty) Seq.empty[Frag]: Frag
        else ol(childs.map(a => li(a.toScalaTags(hasWrapper = true))))
      case Some(ChildrenType.DashList) =>
        if (childs.isEmpty) Seq.empty[Frag]: Frag
        else ul(`class` := "dashed", childs.map(a => li(a.toScalaTags(hasWrapper = true))))
      case _ =>
        if (childs.isEmpty) Seq.empty[Frag]: Frag
        else ul(childs.map(a => li(a.toScalaTags(hasWrapper = true))))
    }
    attribute(ContentType) match {
      case Some(ContentType.Heading(h)) =>
        Seq(tag(s"h$h")(contentWithoutP), children)
      case Some(ContentType.Cite) =>
        blockquote(
          children,
          cite(contentWithoutP)
        )
      case Some(ContentType.Hr) =>
        Seq(hr, children)
      case _ =>
        if (hasWrapper)
          Seq(contentWithoutP, children)
        else
          Seq(p(contentWithoutP), children)
    }
  }

  def toScalaTags: Frag = {
    toScalaTags(false)
  }
}

object Node extends DataObject[Node] {
  def toHtml(a: Seq[Node]): String =
    if (a.isEmpty) ""
    else if (a.size == 1) a.head.toScalaTags.render
    else ul(a.map(a => li(a.toScalaTags))).render


  def matchNodeRef(url: String): Option[String] = if (url.startsWith(NodeRefScheme)) Some(url.substring(NodeRefScheme.length + "node?node=".length)) else None

  def nodeRefRelative(nodeId: String): String = {
    val query = s"?node=$nodeId"
    s"node$query"
  }

  def nodRef(nodeId: String): String = {
    s"$NodeRefScheme${nodeRefRelative(nodeId)}"
  }

  val NodeRefScheme = "cherrytree://"

  sealed trait ChildrenType {

  }
  object ChildrenType extends NodeTag[ChildrenType] {
    case object Paragraphs extends ChildrenType {
      override def toString: String = "paragraphs"
    }
    case object OrderedList extends ChildrenType {
      override def toString: String = "ordered list"
    }
    case object UnorderedList extends ChildrenType {
      override def toString: String = "bullet list"
    }
    case object DashList extends ChildrenType {
      override def toString: String = "dash list"
    }

    override private[model] val name = "ChildrenType"

    override private[model] def parse(a: String) =
      a match {
        case "0" => Paragraphs
        case "1" => OrderedList
        case "2" => UnorderedList
        case "3" => DashList
        case _ => Paragraphs
      }

    override private[model] def serialize(t: ChildrenType) = t match {
      case Paragraphs => "0"
      case OrderedList => "1"
      case UnorderedList => "2"
      case DashList => "3"
    }
  }

  sealed trait ContentType {
    def preferredChildrenType: Option[ChildrenType] = None
  }
  object ContentType extends NodeTag[ContentType] {
    case object Cite extends ContentType {
      override def preferredChildrenType: Option[ChildrenType] = Some(ChildrenType.Paragraphs)
      override def toString: String = "cite"
    }
    case object Hr extends ContentType {
      override def toString: String = "hr"
    }
    case class Heading(i: Int) extends ContentType {
      override def preferredChildrenType: Option[ChildrenType] = Some(ChildrenType.Paragraphs)

      override def toString: String = s"heading $i"
    }

    override private[model] val name = "ContentType"

    override private[model] def parse(a: String) =
      if (a.startsWith("h")) Heading(a.substring(1).toInt)
      else if (a == "cite") Cite
      else if (a == "br") Hr // history reason
      else throw new IllegalStateException("Not possible")

    override private[model] def serialize(t: ContentType) = t match {
      case Heading(a) => "h" + a
      case Cite => "cite"
      case Hr => "br"  // history reason
    }
  }
  def cloneNodes(n: Seq[Node]): Seq[Node] = {
    n.map(_.cloneNode())
  }

  def defaultMode(root: Node, node: model.cursor.Node, enableModal: Boolean): mode.Node.Content = {
    model.mode.Node.Content(node, root(node).content.defaultMode(enableModal))
  }

  val debug_empty = Node("", Content.Rich(Rich.empty), Map.empty, Seq.empty)

  def create(content: Content = Content.Rich(Rich.empty)): Node =  Node(UUID.randomUUID().toString, content, Map.empty, Seq.empty)



  val pickler: Pickler[Node] = new Pickler[Node] {
    override def pickle(obj: Node)(implicit state: PickleState): Unit = {
      import state.enc._
      writeString(obj.uuid)
      Content.pickler.pickle(obj.content)
      writeInt(obj.attributes.size)
      for (c <- obj.attributes) {
        if (c._2.nonEmpty) {
          writeString(c._1)
          writeString(c._2)
        }
      }
      writeInt(obj.childs.size)
      for (c <- obj.childs) Node.pickler.pickle(c)
    }

    override def unpickle(implicit state: UnpickleState): Node = {
      import state.dec._
      Node(
        readString,
        Content.pickler.unpickle,
        (0 until readInt).map(_ => readString -> readString).toMap,
        (0 until readInt).map(_ => Node.pickler.unpickle))
    }
  }

  override def random(r: Random): Node = randomWithDepth(r, 0)

  private def randomWithDepth(r: Random, depth: Int): Node = {
    val childsAtDepth = depth match {
      case 0 => 5
      case 1 => 4
      case 2 => 4
      case 3 => 2
      case _ => 1
    }
    Node(r.nextInt.toString, Content.random(r),
      Map.empty,
      (0 until r.nextInt(childsAtDepth)).map(_ => randomWithDepth(r, depth + 1)))
  }
}
