package model.data

import java.util.UUID

import model._
import Node.{ChildrenType, ContentType, IgnoreInSearch, Priority}
import boopickle.BasicPicklers
import model.range.IntRange
import play.api.libs.json._
import scalatags.Text.all._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

trait NodeTag[T] {
  private[model] val name: String
  private[model] def parse(a: JsValue): T
  private[model] def serialize(t: T): JsValue
}


// LATER simple type of node, so that it can be article, ordered list, unordered list, quote
case class Node(
  uuid: UUID,
  content: Content,
  attributes: JsObject,
  childs: Seq[Node]) {

  private def collectIds(map: mutable.Map[UUID, UUID]): Unit = {
    map.put(uuid, UUID.randomUUID())
    childs.foreach(a => a.collectIds(map))
  }

  def mapBy(map: Map[UUID, UUID]): Node = {
    copy(uuid = map(uuid), content = content.mapBy(map), childs = childs.map(a => a.mapBy(map)))
  }

  def regenerateIds(): Node = {
    val map = mutable.Map[UUID, UUID]()
    collectIds(map)
    mapBy(map.toMap)
  }


  def assertNewNodes(strings: Seq[UUID]): Unit = {
    if (strings.contains(uuid)) {
      throw new IllegalStateException("Not allowed")
    }
    childs.foreach(_.assertNewNodes(strings))
  }

  def isLaTeXMacro: Boolean =
    content match {
      case c: Content.Code if c.lang == LaTeXMacro =>
        true
      case _ =>
        false
    }

  def map(c: model.cursor.Node, transform: Node => Node): Node = {
    var noTagChange = false
    var noMacroChange = false
    def inner(n: Node, c: model.cursor.Node): Node = {
      if (c.isEmpty) {
        val before = n
        val after = transform(n)
        if (before.allTags_ != null &&  (if (before.childs == after.childs) before.selfTags == after.selfTags else before.allTags == after.allTags)) {
          noTagChange = true
        }
        if (macros_ != null && before.macros == after.macros) {
          noMacroChange = true
        }
        after
      } else {
        val before = n
        val after = before.copy(childs = before.childs.patch(c.head, Seq(inner(before.childs(c.head), c.tail)), 1))
        if (noTagChange) {
          after.allTags_ = before.allTags_
        }
        if (noMacroChange) {
          after.macros_ = before.macros_
        }
        after
      }
    }
    inner(this, c)
  }


  lazy val selfTags : Map[Text.HashTag, Int]  = content match {
    case Content.Rich(rich) => rich.tags
    case _ => Map.empty
  }

  private var allTags_ : Map[Text.HashTag, Int] = null

  private def addMaps(col: mutable.Map[Text.HashTag, Int], allTags: Map[Text.HashTag, Int]): Unit = {
    for (m <- allTags) {
      col.get(m._1) match {
        case None => col.put(m._1, m._2)
        case Some(j) => col.put(m._1, m._2 + j)
      }
    }
  }

  def allTags: Map[Text.HashTag, Int] = {
    if (allTags_ == null) {
      if (childs.isEmpty) {
        allTags_ = selfTags
      } else {
        val col = mutable.Map[Text.HashTag, Int]()
        for (c <- childs) {
          addMaps(col, c.allTags)
        }
        addMaps(col, selfTags)
        allTags_ = col.toMap
      }
    }
    allTags_
  }


  def macros: Seq[String] = {
    if (macros_ == null) {
      var cur: String = null
      content match {
        case c: Content.Code if c.lang == LaTeXMacro =>
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

  def refOfThis() = Node.nodRef(uuid)

  def count: Int = 1 + childs.map(_.count).sum
  def size: Int = content.size + childs.map(_.size).sum

  def findB(pred: Node => Boolean, currentCur: model.cursor.Node): Option[model.cursor.Node] = {
    val kk = pred(this)
    if (kk) {
      Some(currentCur)
    } else {
      childs.zipWithIndex.foreach(a =>  {
        val res = a._1.findB(pred, currentCur :+ a._2)
        if (res.isDefined) {
          return res
        }
      })
      None
    }
  }

  def find[T](pred: Node => Option[T], currentCur: model.cursor.Node): Option[(model.cursor.Node, T)] = {
    val kk = pred(this)
    if (kk.isDefined) {
      Some((currentCur, kk.get))
    } else {
      childs.zipWithIndex.foreach(a =>  {
        val res = a._1.find(pred, currentCur :+ a._2)
        if (res.isDefined) {
          return res
        }
      })
      None
    }
  }

  def lookup(_2: UUID, currentCur: model.cursor.Node = model.cursor.Node.root): Option[model.cursor.Node] = {
    findB(_.uuid == _2, currentCur)
  }


  private def filter0(cur: model.cursor.Node, cutAll: Node => Boolean, a: Node => Boolean, bf: ArrayBuffer[model.cursor.Node]): Unit = {
    if (!cutAll(this)) {
      if (a(this)) bf.append(cur)
      childs.zipWithIndex.foreach(pair => {
        pair._1.filter0(cur :+ pair._2, cutAll, a, bf)
      })
    }
  }

  def filter(cur: model.cursor.Node, cutAlls: Node => Boolean, a: Node => Boolean): Seq[model.cursor.Node] = {
    val bf = new ArrayBuffer[model.cursor.Node]()
    filter0(cur, a, cutAlls, bf)
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

  def allChildrenUuids(cur: model.cursor.Node, in: Map[UUID, Boolean]): Seq[model.cursor.Node] = {
    childs.zipWithIndex.flatMap(c => {
      val cs = c._1.allChildrenUuids(cur :+ c._2, in)
      if (in.get(c._1.uuid).contains(true)) cs :+ (cur :+ c._2) else cs
    })
  }

  def cloneNode(): Node = copy(uuid = UUID.randomUUID(), childs = Node.cloneNodes(childs))


  def has[T](t: NodeTag[T]): Boolean = attributes.value.get(t.name).exists(_ != JsNull)
  def has(t: String): Boolean = attributes.value.get(t).exists(_ != JsNull)

  def clear[T](a: NodeTag[T]) : Node = copy(attributes = attributes - a.name)
  def clear(a: String) : Node = copy(attributes = attributes - a)


  def attribute[T](t: NodeTag[T], a: Option[T]): Node = a match {
    case Some(a) => attribute(t, a)
    case None => clear(t)
  }

  def attribute[T](t: NodeTag[T], a: T): Node = copy(attributes = attributes.+((t.name, t.serialize(a))))
  def attribute(t: String, a: JsValue): Node = if (a == JsNull) copy(attributes = attributes - t) else copy(attributes = attributes.+((t, a)))

  def attribute[T](a: NodeTag[T]): Option[T] = attributes.value.get(a.name).filter(_ != JsNull).map(a.parse)
  def attribute(a: String): JsValue = attributes.value.getOrElse(a, JsNull)

  def contentType: Option[ContentType] = attribute(ContentType)

  def priority: Option[Int] = attribute(Priority)

  def ignoreInSearch: Boolean = attribute(IgnoreInSearch).isDefined


  def rich : Rich = content.asInstanceOf[Content.Rich].content


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
    def contentWithoutP = content.toScalaTags(false)
    def children: Frag = attribute(ChildrenType) match {
      case Some(ChildrenType.Paragraphs) =>
        childs.map(_.toScalaTags)
      case Some(ChildrenType.OrderedList) =>
        if (childs.isEmpty) Seq.empty[Frag]: Frag
        else ol(childs.map(a => li(a.toScalaTags(hasWrapper = true))))
      case Some(ChildrenType.DashList) =>
        if (childs.isEmpty) Seq.empty[Frag]: Frag
        else ul(cls := "dashed", childs.map(a => li(a.toScalaTags(hasWrapper = true))))
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


  // TODO support multi document ref
  def matchNodeRef(url: String): Option[UUID] = if (url.startsWith(NodeRefScheme)) Some(UUID.fromString(url.substring(NodeRefScheme.length + "?node=".length))) else None

  def nodeRefRelative(nodeId: UUID): String = {
    val query = s"?node=$nodeId"
    s"$query"
  }

  def nodRef(nodeId: UUID): String = {
    s"$NodeRefScheme${nodeRefRelative(nodeId)}"
  }

  val NodeRefScheme = "cherrytree://"


  object Priority extends NodeTag[Int] {
    override private[model] val name = "priority"

    override private[model] def parse(a: JsValue) = a match {
      case number: JsNumber => number.value.intValue()
      case _ => 0
    }

    override private[model] def serialize(t: Int) = JsNumber(t)
  }

  sealed trait ChildrenType {

  }

  object IgnoreInSearch extends NodeTag[Unit] {
    override private[model] val name = "ignore_in_search"

    override private[model] def parse(a: JsValue): Unit = Unit

    override private[model] def serialize(t: Unit): JsValue = JsString("1")
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

    override private[model] val name = "children_type"

    override private[model] def parse(aa: JsValue) = aa match {
      case JsString(a) =>
        a match {
          case "p" => Paragraphs
          case "ol" => OrderedList
          case "ul" => UnorderedList
          case "dl" => DashList
          case _ => Paragraphs
        }
      case _ => Paragraphs
    }

    override private[model] def serialize(t: ChildrenType) = JsString(t match {
      case Paragraphs => "p"
      case OrderedList => "ol"
      case UnorderedList => "ul"
      case DashList => "dl"
    })
  }

  sealed trait ContentType {
    def preferredChildrenType: Option[ChildrenType] = None
  }
  object ContentType extends NodeTag[ContentType] {
    case object Cite extends ContentType {
      override def toString: String = "cite"
    }
    case object Hr extends ContentType {
      override def toString: String = "hr"
    }
    case class Heading(i: Int) extends ContentType {
      override def toString: String = s"heading $i"
    }

    override private[model] val name = "content_type"

    override private[model] def parse(aa: JsValue) = aa match {
      case JsString(a) =>
        if (a == "hr" || a == "br") Hr
        else if (a.startsWith("h")) Heading(a.substring(1).toInt)
        else if (a == "cite") Cite
        else throw new IllegalStateException("Not possible")
      case _ => throw new IllegalStateException("Not possible")
    }

    override private[model] def serialize(t: ContentType) = JsString(t match {
      case Heading(a) => "h" + a
      case Cite => "cite"
      case Hr => "hr"
    })
  }
  def cloneNodes(n: Seq[Node]): Seq[Node] = {
    n.map(_.cloneNode())
  }

  def defaultMode(root: Node, node: model.cursor.Node, enableModal: Boolean): mode.Node.Content = {
    model.mode.Node.Content(node, root(node).content.defaultMode(enableModal))
  }

  val debug_empty = Node(UUID.randomUUID(), Content.Rich(Rich.empty), JsObject.empty, Seq.empty)

  def create(content: Content = Content.Rich(Rich.empty)): Node =  Node(UUID.randomUUID(), content, JsObject.empty, Seq.empty)

  def create(title: String): Node =  create(Content.Rich(Rich(Seq(Text.Plain(Unicode(title))))))


  val jsonFormat: Format[Node] = Json.format[Node]


  val pickler: Pickler[Node] = new Pickler[Node] {
    override def pickle(obj: Node)(implicit state: PickleState): Unit = {
      import state.enc._
      BasicPicklers.UUIDPickler.pickle(obj.uuid)
      Content.pickler.pickle(obj.content)
      model.jsonObjectPickler.pickle(obj.attributes)
      writeInt(obj.childs.size)
      for (c <- obj.childs) Node.pickler.pickle(c)
    }


    override def unpickle(implicit state: UnpickleState): Node = {
      import state.dec._
      Node(
        BasicPicklers.UUIDPickler.unpickle,
        Content.pickler.unpickle,
        model.jsonObjectPickler.unpickle,
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
    Node(UUID.randomUUID(), Content.random(r),
      JsObject.empty,
      (0 until r.nextInt(childsAtDepth)).map(_ => randomWithDepth(r, depth + 1)))
  }
}
