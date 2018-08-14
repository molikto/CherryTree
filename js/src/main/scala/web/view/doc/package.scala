package web.view

package object doc {


  def classesFromNodeAttribute(node: model.data.Node): String = {
    "ct-d-box " + (node.attribute(model.data.Node.ContentType).map {
      case model.data.Node.ContentType.Cite => "ct-d-cite"
      case model.data.Node.ContentType.Br => "ct-d-br"
      case model.data.Node.ContentType.Heading(j) => if (j > 1) s"ct-d-heading ct-d-h$j" else s"ct-d-h$j"
      case _ => ""
    }.getOrElse("") + " " + node.attribute(model.data.Node.ChildrenType).map {
      case model.data.Node.ChildrenType.UnorderedList => "ct-d-ul"
      case model.data.Node.ChildrenType.OrderedList => "ct-d-ol"
      case model.data.Node.ChildrenType.Paragraphs => "ct-d-ps"
      case model.data.Node.ChildrenType.DashList => "ct-d-dl"
      case _ => ""
    }.getOrElse(""))
  }
}
