package web

import model.data
import model.data.{Content, Text, Unicode}
import web.view._
import org.scalajs.dom.window
import web.interop.CommonMark

import scala.collection.mutable.ArrayBuffer

package object util {



  def parseFromCommonMarkMarkdown(a: String): data.Node = {
    val root = new CommonMark.Parser(jsObject(a => {
      a.smart = true
    })).parse(a)
    window.console.log(root)
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
            buffer.append(data.Text.Code(Unicode(c.literal))) // TODO
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
            buffer.append(data.Node.create().copy(content = Content.Code(Unicode(c.literal), "source/" + c.info)))
            c = c.next
          case "html_block" =>
            assert(c.firstChild == null, "html_block has child")
            buffer.append(data.Node.create().copy(content = Content.Code(Unicode(c.literal), "embedded/html")))
            c = c.next
          case "thematic_break" =>
            assert(c.firstChild == null)
            buffer.append(data.Node.create().attribute(data.Node.ContentType, data.Node.ContentType.Br))
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
