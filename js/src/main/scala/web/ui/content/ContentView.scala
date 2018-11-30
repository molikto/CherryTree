package web.ui.content

import api.PermissionLevel
import model.data
import model.data.{Embedded, NodeType, SourceCode}
import model.range.IntRange
import org.scalajs.dom
import org.scalajs.dom.raw
import org.scalajs.dom.raw.HTMLElement
import scalatags.JsDom.all.{cls, s}
import view.EditorInterface
import web.view.View
import web.ui.doc.{DocumentView, LaTeXMacroCache}


object ContentView {
  type General = ContentView[data.Content, model.operation.Content]

  trait Code extends ContentView[data.Content.Code, model.operation.Content.Code]

  trait Rich extends ContentView[data.Content.Rich, model.operation.Content.Rich]

}

trait ContentViewCreator {
  import ContentView._

  def contentViewCreatedInDocument = false

  def latexMacroCache: LaTeXMacroCache

  def conentViewFromCode(a: data.Content.Code): Code = {
    a.lang match {
      case Embedded.HTML =>
        new EmbeddedHtmlView(a)
      case Embedded.LaTeX =>
        new EmbeddedLaTeXView(a, latexMacroCache)
      case _ =>
        new SourceView(a)
    }
  }

  def contentViewMatches(a: data.CodeType, v: Code): Boolean = {
    a match {
      case Embedded.HTML => v.isInstanceOf[EmbeddedHtmlView]
      case Embedded.LaTeX => v.isInstanceOf[EmbeddedLaTeXView]
      case _ => v.isInstanceOf[SourceView]
    }
  }


  def findParentContentView(t0: raw.Node, dom: HTMLElement): ContentView.General = {
    var t = t0
    while (t != null && t != dom) {
      View.maybeDom[View](t) match {
        case Some(a)  =>
          val contentView: ContentView.General = a match {
            case view: RichView =>
              view.asInstanceOf[ContentView.General]
            case view: WrappedCodeView =>
              view.asInstanceOf[ContentView.General]
            case _ =>
              null
          }
          if (contentView != null) {
            return contentView
          }
        case _ =>
      }
      t = t.parentNode
    }
    null
  }

  def contentViewMatches(a: data.Content, v: General): Boolean = {
    (a, v.asInstanceOf[Any]) match {
      case (data.Content.Rich(r), v: RichView)  => true
      case (c: data.Content.Code, v: Code) => contentViewMatches(c.lang, v)
      case _ => false
    }
  }


  /**
    *
    * @param editableInDoc should NOT change for a NODE for a user
    */
  def contentViewCreate(a: data.Content, editableInDoc: Boolean = false): General = {
    (a match {
      case r: data.Content.Rich => new RichView(r, contentViewCreatedInDocument && editableInDoc, latexMacroCache)
      case s: data.Content.Code =>
        if (contentViewCreatedInDocument) {
          new WrappedCodeView(s, latexMacroCache)
        } else {
          conentViewFromCode(s)
        }
    }).asInstanceOf[General]
  }


  val docFramerIsSmall = 0

  private val sb = new StringBuilder()
  def classesFromNodeAttribute(folderType: NodeType, node: model.data.Node, nodeType: NodeType): String = {
    sb.setLength(0)
    sb.append("ct-d-")
    sb.append(nodeType.id)
    if (nodeType.isFolder) sb.append(" ct-folder")
    if (nodeType.isHeading != 0) {
      val j = node.heading.getOrElse(7)
      sb.append(if (docFramerIsSmall == 0) {
        if (j > 1) s" ct-d-h$j" else s" ct-d-h1"
      } else if (docFramerIsSmall == 1) {
        if (j > 1) s" ct-d-hs${if (j >= 4) "s" else j.toString}" else s" ct-d-hs1"
      } else {
        if (j > 1) "" else s" ct-d-h1"
      })
    }
    if (nodeType.isList(folderType)) {
      sb.append(node.attribute(model.data.Node.ListType).getOrElse(model.data.Node.ListType.UnorderedList) match {
        case model.data.Node.ListType.OrderedList => " ct-d-ol"
        case model.data.Node.ListType.DashList => " ct-d-dl"
        case _ => " ct-d-ul"
      })
    }
    sb.append(node.priority.getOrElse(0) match {
      case a if a <= -3 => " ct-p-n3plus"
      case -2 => " ct-p-n2"
      case -1 => " ct-p-n1"
      case 0 => ""
      case 1 => " ct-p-1"
      case 2 => " ct-p-2"
      case 3 => " ct-p-3"
      case 4 => " ct-p-4"
      case _ => " ct-p-5plus"
    })
    sb.toString()
  }

}

trait ContentView[T <: data.Content, O <: model.operation.Content] extends View {
  def rangeAroundLine(line: Int, xPos: Int, insert: Boolean): Option[IntRange] = None

  def refreshLaTeX(): Unit  = {}

  def constructVisualLineBuff(): Unit = {}

  def readVisualSelectionLine(selection: raw.Range, isUp: Boolean): Int = 0

  def visualLineCount() = 1

  def clearVisualLineBuff(): Unit = {}

  def createEditor(documentView: DocumentView, controller: EditorInterface): ContentViewEditor.General

  def tempEditableTempDuringSelectionChange(editable: Boolean): Unit = {}

  private var contentData_ : T = null.asInstanceOf[T]

  final def contentData: T = contentData_

  protected def setInitialContent(a: T) = contentData_ = a

  final def updateContent(c: T, trans: O, viewUpdated: Boolean): Unit = {
    contentData_ = c
    onUpdateContent(c, trans, viewUpdated)
  }

  final def updateContent(c: T): Unit = {
    contentData_ = c
    onUpdateContent(c)
  }

  protected def onUpdateContent(c: T, trans: O, viewUpdated: Boolean): Unit = {
    if (!viewUpdated) {
      onUpdateContent(c)
    }
  }



  protected def onUpdateContent(c: T): Unit = {

  }
}
