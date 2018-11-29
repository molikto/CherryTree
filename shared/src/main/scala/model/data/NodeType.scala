package model.data
import play.api.libs.json.{JsString, JsValue}




sealed trait NodeType {
  def id: String
  def name: String
  def allowContent: Boolean = true
  def isFolder = isHeading == 2
  def isHeading: Int = 0
  def isList(folderType: NodeType): Boolean = false
  def shouldFollow: Boolean = false
  def allowedChildrenType(folderType: NodeType): Seq[NodeType] = Seq.empty
  def defaultChildrenType(folderType: NodeType) = allowedChildrenType(folderType).headOption.getOrElse(folderType.allowedChildrenType(folderType).head)

  NodeType.all = NodeType.all :+ this
}

object NodeType extends NodeTag[NodeType] {

  private var all = Seq.empty[NodeType]
  lazy val folders = all.filter(_.isHeading == 2)

  private lazy val ps: Seq[NodeType] = Seq(Paragraph, Heading, Article, Block, List, Divider, Outliner)
  lazy val lis: Seq[NodeType] = Seq(Li, LiParagraphs)
  lazy val ois: Seq[NodeType] = Seq(Li, Heading)

  // these are defaults
  trait Passive extends NodeType {

  }

  case object Article extends NodeType {
    override def id: String = "article"
    override def name: String = "article"
    override def isHeading: Int = 2
    override def allowedChildrenType(folderType: NodeType) = ps
  }


  case object Outliner extends NodeType {
    override def id: String = "outliner"
    override def name: String = "outliner"
    override def isHeading: Int = 2
    override def allowedChildrenType(folderType: NodeType) = ois
    override def isList(folderType: NodeType): Boolean = true
  }


  case object Heading extends NodeType {
    override def id: String = "heading"
    override def name: String = "heading"
    override def isHeading: Int = 1
    override def shouldFollow = true
    override def allowedChildrenType(folderType: NodeType) =
      if (folderType == Article) ps else ois
    override def isList(folderType: NodeType): Boolean = folderType == Outliner
  }

  case object Block extends NodeType {
    override def id: String = "block"
    override def name: String = "block"
    override def allowedChildrenType(folderType: NodeType) = ps
  }

  case object Paragraph extends NodeType with Passive {
    override def id: String = "paragraph"
    override def name: String = "paragraph"
    override def allowContent: Boolean = true
    override def allowedChildrenType(folderType: NodeType) = Seq.empty
  }

  case object Divider extends NodeType {
    override def id: String = "divider"
    override def name: String = "divider, rule, hr"
    override def allowContent: Boolean = false
  }


  case object List extends NodeType {
    override def id: String = "list"
    override def name: String = "list"
    override def allowContent: Boolean = false
    override def allowedChildrenType(folderType: NodeType) = lis
    override def isList(folderType: NodeType): Boolean = true
  }

  case object Li extends NodeType with Passive {
    override def id: String = "li"
    override def name: String = "list item, li"
    override def allowedChildrenType(folderType: NodeType) =
      if (folderType == Article) lis else ois
    override def isList(folderType: NodeType): Boolean = true
  }

  case object LiParagraphs extends NodeType {
    override def id: String = "lip"
    override def name: String = "multi-paragraph list item"
    override def allowContent: Boolean = false
    override def allowedChildrenType(folderType: NodeType) = ps
  }





  override private[model] val name = "type"

  override private[model] def parse(aa: JsValue) = aa match {
    case JsString(a) => all.find(_.id == a).get
    case _ => throw new IllegalStateException("Not possible")
  }

  override private[model] def serialize(t: NodeType) = JsString(t.id)
}
