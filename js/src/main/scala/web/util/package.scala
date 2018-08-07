package web

import model.data
import web.view._
import org.scalajs.dom._
import web.interop.CommonMark

package object util {


  def parseFromMarkdown(a: String): data.Node = {
    val root = new CommonMark.Parser(jsObject(a => {
      a.smart = true
    })).parse(a)
    window.console.log(root)
    data.Node.create()
  }
}
