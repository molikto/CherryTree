package web.view

import org.scalajs.dom.raw.HTMLElement

import client.Client
import _root_.doc.DocTransaction
import client.Client.ViewMessage
import model.cursor
import model.cursor.Node
import org.scalajs.dom.raw._
import scalatags.JsDom.all.{tag, _}
import settings.Settings
import web.view.content.ContentView

package object doc {


  def contentViewFromWithHold(a: HTMLElement): ContentView.General = {
    View.fromDom[ContentView.General](a.childNodes(0).childNodes(0))
  }
  def contentViewAndHold(node: model.data.Node): HTMLElement = {
    div(
      display := "flex",
      flexDirection := "row",
      div(
        `class` := doc.classesFromNodeAttribute(node),
        ContentView.create(node.content)
      ),
      tag("i")(`class` := "ct-d-hold")
    ).render
  }

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
