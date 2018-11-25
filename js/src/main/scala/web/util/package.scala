package web

import java.util.regex.Pattern

import model.data
import model.data.{Content, SourceCode, Text, Unicode, Embedded}
import web.view._
import org.scalajs.dom._
import org.scalajs.dom.raw.{HTMLElement, HTMLImageElement, HTMLLinkElement, HTMLTextAreaElement}
import register.Registerable
import web.interop.CommonMark

import scala.collection.mutable.ArrayBuffer
import scala.scalajs.js

package object util {

  private def fallbackCopyTextToClipboard(text: String) {
    val textArea = document.createElement("textarea").asInstanceOf[HTMLTextAreaElement]
    textArea.value = text
    document.body.appendChild(textArea)
    textArea.focus()
    textArea.select()

    document.execCommand("copy")
    document.body.removeChild(textArea)
  }

  def copyTextToClipboard(text: String) = {
    if (!js.Object.hasProperty(window.navigator, "clipboard")) {
      fallbackCopyTextToClipboard(text)
    } else {
      window.navigator.asInstanceOf[js.Dynamic].clipboard.writeText(text)
    }
  }


  private lazy val detachedDoc = document.implementation.createHTMLDocument("title")

  def readHTML(html0: String): Element = {
    var html = html0
    val doc = detachedDoc
    val elt = doc.createElement("div")
    elt.innerHTML = html
    if (elt.firstChild.nodeName == "meta") removeFromParent(elt.firstChild)
    removeFromParent(elt)
    elt
  }

 def formatDate(a: Long) : String = {
   new js.Date(a.toDouble).asInstanceOf[js.Dynamic].toLocaleString("en-US", jsObject(a => {
     a.hour12 = false
     a.year = "numeric"
     a.month = "short"
     a.day = "numeric"
     a.hour = "2-digit"
     a.minute = "2-digit"
   })).asInstanceOf[String]
 }

  def parseFromHtml(a: String): Registerable = {
    val root = readHTML(a)
    if (model.debug_view) window.console.log("parsing HTML", root)

    def isHeadingTag(nodeName: String): Boolean = nodeName.length == 2 && nodeName.startsWith("H")  && nodeName.charAt(1).isDigit
    def headingLevel(nodeName: String): Int = nodeName.charAt(1) - '0'

    def collectTexts(c0: raw.Node): (raw.Node, Seq[Text]) = {
      var c = c0
      val buffer = new ArrayBuffer[data.Text]()
      def appendPlain(a: String): Unit = {
        if (buffer.nonEmpty && buffer.last.isPlain) {
          val p = buffer.remove(buffer.size - 1)
          if (a == " " && p.asPlain.unicode.str.endsWith(" ")) {
            buffer.append(p)
          } else {
            buffer.append(data.Text.Plain(Unicode(p.asPlain.unicode.str + a)))
          }
        } else {
          buffer.append(data.Text.Plain(Unicode(a)))
        }
      }
      while (c != null) {
        var nextSibling = true
        c.nodeName match {
          case "#text" =>
            var a = c.textContent
            while (c.nextSibling != null && c.nextSibling.nodeName == "#text") {
              c = c.nextSibling
              a = a + c.textContent
            }
            var j = 0
            var i = 0
            while (i >= 0 && j >= 0) {
              i = a.indexOf('$')
              if (i >= 0) {
                j = a.indexOf('$', i + 1)
                if (j >= 0) {
                  if (i > 0) {
                    buffer.append(Text.Plain(Unicode(a.substring(0, i))))
                  }
                  buffer.append(Text.LaTeX(Unicode(a.substring(i + 1, j))))
                  a = a.substring(j + 1)
                }
              }
            }
            if (a.nonEmpty) buffer.append(Text.Plain(Unicode(a)))
//          case "softbreak" =>
//            assert(c.firstChild == null)
//            appendPlain(" ")
          case "BR" =>
            appendPlain("\n")
          case "EM" | "I" =>
            val (cc, t) = collectTexts(c.firstChild)
            if (c.asInstanceOf[HTMLElement].style.fontWeight == "bold") {
              buffer.append(data.Text.Strong(t))
            } else {
              buffer.append(data.Text.Emphasis(t))
            }
            if (cc != null) {
              nextSibling = false
              c = cc
            }
          case "STRONG" | "B" | "MARK" =>
            val (cc, t) = collectTexts(c.firstChild)
            buffer.append(data.Text.Strong(t))
            if (cc != null) {
              nextSibling = false
              c = cc
            }
          case "DEL" | "S" =>
            val (cc, t) = collectTexts(c.firstChild)
            buffer.append(data.Text.StrikeThrough(t))
            if (cc != null) {
              nextSibling = false
              c = cc
            }
          case "A" =>
            val (cc, t) = collectTexts(c.firstChild)
            buffer.append(data.Text.Link(t, Unicode(c.asInstanceOf[HTMLLinkElement].href), Unicode(c.asInstanceOf[HTMLLinkElement].title)))
            if (cc != null) {
              nextSibling = false
              c = cc
            }
          case "IMG" =>
            buffer.append(data.Text.Image(Unicode(c.asInstanceOf[HTMLImageElement].src), Unicode(c.asInstanceOf[HTMLImageElement].title)))
          case "CODE" =>
            buffer.append(data.Text.Code(Unicode(c.textContent)))
          case a if isTextNode(a) =>
            val (cc, t) = collectTexts(c.firstChild)
            if (c.asInstanceOf[HTMLElement].style.fontWeight == "bold") {
              buffer.append(data.Text.Strong(t))
            } else {
              buffer.appendAll(t)
            }
            if (cc != null) {
              nextSibling = false
              c = cc
            }
          case a if isBlockNode(a) =>
            return (c, buffer)
          case _ =>
            buffer.append(data.Text.HTML(Unicode(c.asInstanceOf[HTMLElement].outerHTML)))
        }
        if (nextSibling) c = c.nextSibling
      }
      (null, buffer)
    }

    def isBlockNode(a: String) = a match {
      case "UL" | "OL" | "DIV" | "P" | "SECTION" | "BLOCKQUOTE" | "ARTICLE" | "ASIDE" | "DD" | "DL" | "DT" | "FIGCAPTION" | "FIGURE" | "FOOTER" | "HEADER" |
           "HGROUP" | "HR" | "LI" | "MAIN" | "NAV" | "TABLE" | "TFOOT" =>
        true
      case _ => false
    }
    def isTextNode(a: String) = a match {
      case "#text" | "EM" | "I" | "STRONG" | "CODE" | "A" |
           "ABBR" | "ACRONYM" | "SUB" | "SUP" | "LABEL" | "TIME" | "TT" | "Q" | "VAR" | "BIG" | "SMALL" | "SAMP" |
           "IMG" | "SPAN" | "BR" | "B" | "BDI" | "BDO" | "DEL" | "INS" | "KBD" | "MARK" | "DFN"  | "S"  | "METER" =>
        true
      case _  => false
    }

    def collectParagraph(c0: raw.Node): (raw.Node, model.data.Content.Rich) = {
      val (n, t0) = collectTexts(c0)
      val t = if (t0.lastOption.contains(Text.Plain(Unicode("\n")))) {
        t0.dropRight(1)
      } else {
        t0
      }
      (n, model.data.Content.Rich(model.data.Rich(t)))
    }
    def collectTo(buffer: ArrayBuffer[data.Node], content: raw.Node, currentInsideTitle: Int): raw.Node = {
      var c = content
      while (c != null && (!isHeadingTag(c.nodeName) || headingLevel(c.nodeName) > currentInsideTitle)) { // we got a new heading
        c.nodeName match {
          case a if isHeadingTag(a) =>
            val b = new ArrayBuffer[data.Node]()
            val (_, title) = collectParagraph(c.firstChild)
            var bb = data.Node.create().copy(content = title, childs = b).attribute(data.Node.ContentType, data.Node.ContentType.Heading(headingLevel(c.nodeName)))
            if (c.nextSibling != null) {
              c = collectTo(b, c.nextSibling, headingLevel(c.nodeName)) // collect until that
            } else {
              c = null
            }
            if (!bb.has(data.Node.ChildrenType)) {
              bb = bb.attribute(data.Node.ChildrenType, data.Node.ChildrenType.Paragraphs)
            }
            buffer.append(bb)
          case "P" =>
            val (next, t) = collectParagraph(c.firstChild)
            buffer.append(data.Node.create().copy(content = t))
            c = if (next == null) c.nextSibling else next
          case "BLOCKQUOTE" =>
            val b = new ArrayBuffer[data.Node]()
            collectTo(b, c.firstChild, 0) // collect until that
            buffer.append(data.Node.create().copy(childs = b)
              .attribute(data.Node.ContentType, data.Node.ContentType.Cite)
            )
            c = c.nextSibling
          case "PRE" =>
            buffer.append(data.Node.create().copy(content = Content.Code(Unicode(c.textContent), SourceCode("javascript")))) // TODO get source somewhere
            c = c.nextSibling
          case "HR" =>
            buffer.append(data.Node.create().attribute(data.Node.ContentType, data.Node.ContentType.Hr))
            c = c.nextSibling
          case "OL" | "UL" =>
            val b = new ArrayBuffer[data.Node]()
            var cc = c.firstChild
            while (cc != null) {
              val bb = new ArrayBuffer[data.Node]()
              collectTo(bb, cc.firstChild, 0)
              b.append(if (bb.size < 1) data.Node.create() else if (bb.size == 1) bb.head else data.Node.create().copy(childs = bb))
              cc = cc.nextSibling
            }
            val tt = if (buffer.nonEmpty && buffer.last.childs.isEmpty) {
              buffer.remove(buffer.size - 1).copy(childs = b)
            } else {
              data.Node.create().copy(childs = b)
            }
            buffer.append(tt.attribute(data.Node.ChildrenType,
              if (c.nodeName == "OL") data.Node.ChildrenType.OrderedList else data.Node.ChildrenType.UnorderedList))
            c = c.nextSibling
          case "DL" =>
            val b = new ArrayBuffer[data.Node]()
            var cc = c.firstChild
            while (cc != null) {
              val bb = new ArrayBuffer[data.Node]()
              collectTo(bb, cc.firstChild, 0)
              var top = if (bb.size < 1) data.Node.create() else if (bb.size == 1) bb.head else data.Node.create().copy(childs = bb)
              if (top.childs.isEmpty && cc.nodeName == "DT" && cc.nextSibling != null && cc.nextSibling.nodeName == "DD") {
                top = top.copy(childs = Seq(data.Node.create(collectParagraph(cc.nextSibling.firstChild)._2)))
                cc = cc.nextSibling
              }
              b.append(top)
              cc = cc.nextSibling
            }
            val tt = if (buffer.nonEmpty && buffer.last.childs.isEmpty) {
              buffer.remove(buffer.size - 1).copy(childs = b)
            } else {
              data.Node.create().copy(childs = b)
            }
            buffer.append(tt.attribute(data.Node.ChildrenType, data.Node.ChildrenType.UnorderedList))
            c = c.nextSibling
          case "META" | "#comment" |"#cdata-section" =>
            c = c.nextSibling
          case a if isBlockNode(a) =>
            collectTo(buffer, c.firstChild, currentInsideTitle)
            c = c.nextSibling
          case _ if isTextNode(c.nodeName) =>
            val (next, t) = collectParagraph(c)
            buffer.append(data.Node.create().copy(content = t))
            c = next
          case _ if c.isInstanceOf[HTMLElement] =>
            buffer.append(data.Node.create().copy(content = Content.Code(Unicode(c.asInstanceOf[HTMLElement].outerHTML), Embedded.HTML)))
            c = c.nextSibling
          case _ =>
            c = c.nextSibling
        }
      }
      c
    }
    val bf = new ArrayBuffer[data.Node]()
    collectTo(bf, root.firstChild,0)
    if (bf.isEmpty) Registerable.Text(Seq.empty)
    else if (bf.size == 1 && bf.head.childs.isEmpty && bf.head.content.isRich) Registerable.Text(bf.head.rich.text)
    else Registerable.Node(bf, None)
  }




  def parseFromCommonMarkMarkdown(a: String): data.Node = {
    val root = new CommonMark.Parser(jsObject(a => {
      a.smart = true
    })).parse(a)
    if (model.debug_view) window.console.log(root)
    def collectTexts(c0: CommonMark.Node): Seq[data.Text] = {
      var c = c0
      val buffer = new ArrayBuffer[data.Text]()
      def appendPlain(a: String): Unit = {
        if (buffer.nonEmpty && buffer.last.isPlain) {
          val p = buffer.remove(buffer.size - 1)
          if (a == " " && p.asPlain.unicode.str.endsWith(" ")) {
            buffer.append(p)
          } else {
            buffer.append(data.Text.Plain(Unicode(p.asPlain.unicode.str + a)))
          }
        } else {
          buffer.append(data.Text.Plain(Unicode(a)))
        }
      }
      while (c != null) {
        c.`type` match {
          case "text" =>
            assert(c.firstChild == null)
            var a = c.literal
            while (c.next != null && c.next.`type` == "text") {
              c = c.next
              a = a + c.literal
            }
            var j = 0
            var i = 0
            while (i >= 0 && j >= 0) {
              i = a.indexOf('$')
              if (i >= 0) {
                j = a.indexOf('$', i + 1)
                if (j >= 0) {
                  if (i > 0) {
                    buffer.append(Text.Plain(Unicode(a.substring(0, i))))
                  }
                  buffer.append(Text.LaTeX(Unicode(a.substring(i + 1, j))))
                  a = a.substring(j + 1)
                }
              }
            }
            if (a.nonEmpty) buffer.append(Text.Plain(Unicode(a)))
          case "softbreak" =>
            assert(c.firstChild == null)
            appendPlain(" ")
          case "linebreak" =>
            assert(c.firstChild == null)
            appendPlain("\n")
          case "emph" =>
            buffer.append(data.Text.Emphasis(collectTexts(c.firstChild)))
          case "strong" =>
            buffer.append(data.Text.Strong(collectTexts(c.firstChild)))
          case "html_inline" =>
            assert(c.firstChild == null)
            buffer.append(data.Text.HTML(Unicode(c.literal)))
          case "link" =>
            buffer.append(data.Text.Link(collectTexts(c.firstChild), Unicode(c.destination), Unicode(c.title)))
          case "image" =>
            buffer.append(data.Text.Image(Unicode(c.destination), Unicode(c.title)))
          case "code" =>
            assert(c.firstChild == null)
            buffer.append(data.Text.Code(Unicode(c.literal)))
        }
        c = c.next
      }
      buffer
    }

    def collectParagraph(c0: CommonMark.Node): model.data.Content.Rich = {
      model.data.Content.Rich(model.data.Rich(collectTexts(c0)))
    }
    def collectTo(buffer: ArrayBuffer[data.Node], content: CommonMark.Node, currentInsideTitle: Int): CommonMark.Node = {
      var c = content
      while (c != null && (c.`type` != "heading" || c.level > currentInsideTitle)) { // we got a new heading
        c.`type` match {
          case "heading" =>
            val b = new ArrayBuffer[data.Node]()
            val title = collectParagraph(c.firstChild)
            var bb = data.Node.create().copy(content = title, childs = b).attribute(data.Node.ContentType, data.Node.ContentType.Heading(c.level))
            if (c.next != null) {
              c = collectTo(b, c.next, c.level) // collect until that
            }
            if (!bb.has(data.Node.ChildrenType)) {
              bb = bb.attribute(data.Node.ChildrenType, data.Node.ChildrenType.Paragraphs)
            }
            buffer.append(bb)
          case "paragraph" =>
            buffer.append(data.Node.create().copy(content = collectParagraph(c.firstChild)))
            c = c.next
          case "block_quote" =>
            val b = new ArrayBuffer[data.Node]()
            collectTo(b, c.firstChild, 0) // collect until that
            buffer.append(data.Node.create().copy(childs = b)
              .attribute(data.Node.ContentType, data.Node.ContentType.Cite)
            )
            c = c.next
          case "code_block" =>
            assert(c.firstChild == null, "code_block has child")
            buffer.append(data.Node.create().copy(content = Content.Code(Unicode(c.literal), SourceCode(c.info))))
            c = c.next
          case "html_block" =>
            assert(c.firstChild == null, "html_block has child")
            buffer.append(data.Node.create().copy(content = Content.Code(Unicode(c.literal), Embedded.HTML)))
            c = c.next
          case "thematic_break" =>
            assert(c.firstChild == null)
            buffer.append(data.Node.create().attribute(data.Node.ContentType, data.Node.ContentType.Hr))
            c = c.next
          case "list" => // LATER support dash list
            val b = new ArrayBuffer[data.Node]()
            var cc = c.firstChild
            while (cc != null) {
              assert(cc.`type` == "item", s"should have type item but ${cc.`type`}")
              val bb = new ArrayBuffer[data.Node]()
              collectTo(bb, cc.firstChild, 0)
              b.append(if (bb.size < 1) data.Node.create() else if (bb.size == 1) bb.head else data.Node.create().copy(childs = bb))
              cc = cc.next
            }
            val tt = if (buffer.nonEmpty && buffer.last.childs.isEmpty) {
              buffer.remove(buffer.size - 1).copy(childs = b)
            } else {
              data.Node.create().copy(childs = b)
            }
            buffer.append(tt.attribute(data.Node.ChildrenType,
              if (c.listType == "Ordered") data.Node.ChildrenType.OrderedList else data.Node.ChildrenType.UnorderedList))
            c = c.next
        }
      }
      c
    }
    val bf = new ArrayBuffer[data.Node]()
    collectTo(bf, root.firstChild,0)
    if (bf.size == 1) bf.head else data.Node.create().copy(childs = bf)
  }
}
