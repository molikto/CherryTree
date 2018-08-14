package model.data

import java.util.UUID

import doc.DocTransaction
import model._
import model.data.Node.ContentType
import model.range.IntRange

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

trait NodeTag[T] {
  private[model] val name: String
  private[model] def parse(a: String): T
  private[model] def serialize(t: T): String
}

// LATER simple type of node, so that it can be article, ordered list, unordered list, quote
case class Node (
  uuid: String,
  content: Content,
  attributes: Map[String, String],
  childs: Seq[Node]) {
  def lookup(_2: String, currentCur: cursor.Node = cursor.Node.root): Option[cursor.Node] = {
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


  private def filter0(cur: cursor.Node, a: Content => Boolean, bf: ArrayBuffer[cursor.Node]): Unit = {
    if (a(content)) bf.append(cur)
    childs.zipWithIndex.foreach(pair => {
      pair._1.filter0(cur :+ pair._2, a, bf)
    })
  }

  def filter(cur: cursor.Node, a: Content => Boolean): Seq[cursor.Node] = {
    val bf = new ArrayBuffer[cursor.Node]()
    filter0(cur, a, bf)
    bf
  }


  def lastDefined(a: cursor.Node): cursor.Node = {
    var len = a.length
    while (len >= 0) {
      val can = a.take(len)
      if (get(can).isDefined) {
        return can
      }
      len -= 1
    }
    cursor.Node.root
  }

  def isH1: Boolean = attribute(ContentType).contains(ContentType.Heading(1))
  def isHeading: Boolean = attribute(ContentType).exists(_.isInstanceOf[ContentType.Heading])


  def allChildrenUuids(cur: cursor.Node, in: Map[String, Boolean]): Seq[cursor.Node] = {
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


  def rich : Rich = content.asInstanceOf[Content.Rich].content


  def map(c: cursor.Node, transform: Node => Node): Node = {
    if (c.isEmpty) {
      transform(this)
    } else {
      copy(childs = childs.patch(c.head, Seq(childs(c.head).map(c.tail, transform)), 1))
    }
  }

  def get(a: cursor.Node): Option[Node] =
    if (a.isEmpty) Some(this) else if (a.head >= childs.size) None else childs(a.head).get(a.tail)

  def apply(c: cursor.Node): Node = if (c.isEmpty) this else childs(c.head)(c.tail)

  def apply(r: range.Node): Seq[Node] = this(r.parent)(r.childs)

  def apply(r: IntRange): Seq[Node] = childs.slice(r.start, r.until)

  def delete(d: range.Node): Node = map(d.parent, _.delete(d.childs))

  private def delete(r: IntRange): Node =
    copy(childs = childs.patch(r.start, Seq.empty, r.size))

  private def insert(i: Int, cs: Seq[Node]): Node = {
    if (i < 0 || i > childs.size) throw new IllegalArgumentException("Insertion is out of bound")
    copy(childs = childs.patch(i, cs, 0))
  }

  def insert(c: cursor.Node, cs: Seq[Node]): data.Node =
    map(c.dropRight(1), a => a.insert(c.last, cs))

  def move(r: range.Node, at: cursor.Node): data.Node = {
    val a = this(r)
    delete(r).insert(r.transformAfterDeleted(at).get, a)
  }





}

object Node extends DataObject[Node] {

  sealed trait ChildrenType {

  }
  object ChildrenType extends NodeTag[ChildrenType] {
    case object Paragraphs extends ChildrenType
    case object OrderedList extends ChildrenType
    case object UnorderedList extends ChildrenType
    case object DashList extends ChildrenType

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
    }
    case object Br extends ContentType
    case class Heading(i: Int) extends ContentType {
      override def preferredChildrenType: Option[ChildrenType] = Some(ChildrenType.Paragraphs)
    }

    override private[model] val name = "ContentType"

    override private[model] def parse(a: String) =
      if (a.startsWith("h")) Heading(a.substring(1).toInt)
      else if (a == "cite") Cite
      else if (a == "br") Br
      else throw new IllegalStateException("Not possible")

    override private[model] def serialize(t: ContentType) = t match {
      case Heading(a) => "h" + a
      case Cite => "cite"
      case Br => "br"
    }
  }
  def cloneNodes(n: Seq[Node]): Seq[Node] = {
    n.map(_.cloneNode())
  }

  def defaultNormalMode(root: Node, node: cursor.Node): mode.Node.Content = {
    model.mode.Node.Content(node, root(node).content.defaultNormalMode())
  }

  val debug_empty = Node("", data.Content.Rich(data.Rich.empty), Map.empty, Seq.empty)

  def create(): Node =  Node(UUID.randomUUID().toString, data.Content.Rich(data.Rich.empty), Map.empty, Seq.empty)


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
        if (oldDocVersion) UUID.randomUUID().toString
        else readString,
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
    data.Node(r.nextString(10), data.Content.random(r),
      Map.empty,
      (0 until r.nextInt(childsAtDepth)).map(_ => randomWithDepth(r, depth + 1)))
  }
}
