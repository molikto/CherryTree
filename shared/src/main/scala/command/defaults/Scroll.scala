package command.defaults

import command.{CommandCategory, CommandInterface, CommandInterfaceAvailable, Motion}
import command.Key.{Ctrl, KeySeq, PageDown, PageUp}
import doc.{DocState, DocTransaction}
import model.cursor.Node
import model.data.Unicode
import settings.Settings

class Scroll(settings: Settings) extends CommandCategory(settings,"scrolling the page") {


  // LATER only some of should be implemented
  // Q_sc          Scrolling
  //
  //CTRL-E        N  CTRL-E       window N lines downwards (default: 1)
  //CTRL-D        N  CTRL-D       window N lines Downwards (default: 1/2 window)
  //CTRL-F        N  CTRL-F       window N pages Forwards (downwards)
  //CTRL-Y        N  CTRL-Y       window N lines upwards (default: 1)
  //CTRL-U        N  CTRL-U       window N lines Upwards (default: 1/2 window)
  //CTRL-B        N  CTRL-B       window N pages Backwards (upwards)
  //z<CR>            z<CR> or zt  redraw, current line at top of window
  //z.               z.    or zz  redraw, current line at center of window
  //z-               z-    or zb  redraw, current line at bottom of window
  //
  //These only work when 'wrap' is off:
  //zh            N  zh           scroll screen N characters to the right
  //zl            N  zl           scroll screen N characters to the left
  //zH            N  zH           scroll screen half a screenwidth to the right
  //zL            N  zL           scroll screen half a screenwidth to the left

//  new Command with NodeMotionCommand {
//    override val description: String = "scroll down"
//
//    override def defaultKeys: Seq[KeySeq] = Seq(PageDown, "d" + Ctrl)
//
//    override def move(data: DocState, a: Node): Option[Node] = {
//      None
//    }
//  }
//
//  new Command with NodeMotionCommand {
//    override val description: String = "scroll up"
//
//    override def defaultKeys: Seq[KeySeq] = Seq(PageUp, "u" + Ctrl)
//
//    override def move(data: DocState, a: Node): Option[Node] = {
//      None
//    }
//  }
}
