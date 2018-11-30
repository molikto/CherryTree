package model.data
import play.api.libs.json.{JsString, JsValue}


/**
  * a node type attribute is attached to a node, but most of time, not attached
  *
  * but when a node is putting into a context DocState, every node has a node type
  */
sealed trait NodeType {
  def id: String
  def name: String

  def isFolder = isHeading == 2
  def isHeading: Int = 0
  def shouldInsertAtChildOnOpenBellow: Boolean = false
  def contentCanBeCode: Boolean = false
  def isList(folderType: NodeType): Boolean = false
  def shouldFollow: Boolean = false
  def allowedChildrenType(folderType: NodeType): Seq[NodeType] = Seq.empty
  def defaultChildrenType(folderType: NodeType) = allowedChildrenType(folderType).headOption.getOrElse(NodeType.Li)

  NodeType.all = NodeType.all :+ this
}

object NodeType extends NodeTag[NodeType] {

  private var all = Seq.empty[NodeType]
  lazy val folders = all.filter(_.isHeading == 2)

  private lazy val ps: Seq[NodeType] = Seq(Paragraph, Heading, Article, Block, Divider, Outline)
  lazy val lis: Seq[NodeType] = Seq(Li)
  lazy val ois: Seq[NodeType] = Seq(Li, Heading, Outline, Article)

  // these are defaults
  trait Passive extends NodeType {

  }

  case object Article extends NodeType {
    override def id: String = "article"
    override def name: String = "article"
    override def isHeading: Int = 2
    override def allowedChildrenType(folderType: NodeType) = ps
    override def shouldInsertAtChildOnOpenBellow: Boolean = true
  }


  case object Outline extends NodeType {
    override def id: String = "outline"
    override def name: String = "outline"
    override def isHeading: Int = 2
    override def allowedChildrenType(folderType: NodeType) = ois
    override def isList(folderType: NodeType): Boolean = true
    override def shouldInsertAtChildOnOpenBellow: Boolean = true
  }


  case object Heading extends NodeType {
    override def id: String = "heading"
    override def name: String = "heading"
    override def isHeading: Int = 1
    override def shouldFollow = true
    override def allowedChildrenType(folderType: NodeType) =
      if (folderType == Article) ps else ois
    override def isList(folderType: NodeType): Boolean = folderType == Outline
    override def shouldInsertAtChildOnOpenBellow: Boolean = true
  }

  case object Block extends NodeType {
    override def id: String = "block"
    override def name: String = "block"
    override def allowedChildrenType(folderType: NodeType) = ps
    override def shouldInsertAtChildOnOpenBellow: Boolean = true
  }

  case object Paragraph extends NodeType with Passive {
    override def id: String = "paragraph"
    override def name: String = "paragraph"
    override def allowedChildrenType(folderType: NodeType) = lis
    override def contentCanBeCode: Boolean = true
  }

  case object Divider extends NodeType {
    override def id: String = "divider"
    override def name: String = "divider, rule, hr"
  }

  case object Li extends NodeType with Passive {
    override def id: String = "li"
    override def name: String = "list item, li"
    override def allowedChildrenType(folderType: NodeType) =
      if (folderType == Article) lis else ois
    override def isList(folderType: NodeType): Boolean = true
    override def contentCanBeCode: Boolean = true
  }






  override private[model] val name = "content_type"

  override private[model] def parse(aa: JsValue) = aa match {
    case JsString(a) => all.find(_.id == a).getOrElse(Li)
    case _ => throw new IllegalStateException("Not possible")
  }

  override private[model] def serialize(t: NodeType) = JsString(t.id)
}
