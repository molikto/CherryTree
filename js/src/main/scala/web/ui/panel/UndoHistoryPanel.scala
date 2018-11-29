package web.ui.panel

import client.Client
import scalatags.JsDom.all._
import web.view.{UnselectableView, View, removeAllChild}

class UndoHistoryPanel(val client: Client, val doc: View) extends UnselectableView  {


  dom = div(
    minWidth := "150px",
    width := "100%",
    height := "100%",
    overflowY := "scroll",
    cls := "ct-scroll ct-panel",
    padding := "24px",
  ).render



  override def onAttach(): Unit = {
    observe(client.stateUpdates.doOnNext(state => {
      removeAllChild(dom)
      // TODO node doctor
//      state.to.node.foreach((cur, node) => {
//        if (!node.canHaveChilds && node.childs.nonEmpty) {
//          dom.appendChild(button("Node should not have childs",
//            onclick := ((ev: MouseEvent) => {
//              client.goTo(node.uuid)
//              doc.focus()
//            }),
//          ).render)
//        }
//        if (!state.to.canBe(cur, node.contentType)) {
//          dom.appendChild(button("node type is wrong",
//            onclick := ((ev: MouseEvent) => {
//              client.goTo(node.uuid)
//              doc.focus()
//            }),
//          ).render)
//        }
//        node.contentType match {
//          case Some(model.data.ContentType.Heading(a)) =>
//            if (a > 1 && !node.isFolder && cur != model.cursor.Node.root) {
//              if (state.to.node(model.cursor.Node.parent(cur)).heading != Some(a - 1)) {
//                dom.appendChild(button("Heading level wrong",
//                  onclick := ((ev: MouseEvent) => {
//                    client.goTo(node.uuid)
//                    doc.focus()
//                  }),
//                ).render)
//              }
//            }
//          case _ =>
//        }
//        if (!state.to.canBe(cur, node.contentType)) {
//          dom.appendChild(button("node type is wrong",
//            onclick := ((ev: MouseEvent) => {
//              client.goTo(node.uuid)
//              doc.focus()
//            }),
//          ).render)
//        }
//        if (!state.to.childCanBeLists(cur) && node.childIsLists) {
//          dom.appendChild(button("List type wrong",
//            onclick := ((ev: MouseEvent) => {
//              client.goTo(node.uuid)
//              doc.focus()
//            }),
//          ).render)
//        }
//      }, model.cursor.Node.root)
      client.debug_undoHistory.takeRight(30).foreach(a =>
        dom.appendChild(
          div(
            marginBottom := "8px",
            div(span(a._1), " ", span(a._3)),
            a._2.map(c => div(c.toString))
          ).render
        )
      )
      scrollToBottom()
    }))
  }
}
