package web.ui.panels

import client.Client
import scalatags.JsDom.all._
import web.view.{UnselectableView, removeAllChild}

class UndoHistoryPanel(val client: Client) extends UnselectableView  {


  dom = div(
    minWidth := "150px",
    width := "100%",
    height := "100%",
    overflowY := "scroll",
    `class` := "ct-scroll ct-panel",
    padding := "24px",
  ).render




  observe(client.stateUpdates.map(_ => 0).doOnNext(pair => {
    removeAllChild(dom)
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
