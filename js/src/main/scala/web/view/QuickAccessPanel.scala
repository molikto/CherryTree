package web.view


import client.Client
import command.Key
import monix.execution.Ack
import scalatags.JsDom.all._
import org.scalajs.dom.raw.{HTMLElement, MouseEvent}
import util.QuickDiff
import web.view.content.ContentListViewQuickDiff

import scala.concurrent.Future

class QuickAccessPanel(client: Client) extends UnselectableView {


  private val parentsView = div(
  ).render

  dom = div(
    minWidth := "150px",
    width := "100%",
    height := "100%",
    overflowY := "scroll",
    `class` := "ct-scroll ct-panel ct-document-style ",
    padding := "24px",
    parentsView
  ).render


  private def state = client.state

  observe(client.stateUpdates.doOnNext(update => {
    renderState(update.from.isEmpty)
  }))

  private var previousZoom: model.cursor.Node = null
  private val parentDiff = new ContentListViewQuickDiff(parentsView)

  def renderState(emptyDataChange: Boolean): Unit = {
    val zoom = state.zoom
    if (emptyDataChange && previousZoom == zoom) {
      // no changes
    } else {
      val curParents = zoom.indices.map(l => state.node(zoom.take(l))).toArray
      parentDiff.update(curParents)
    }
  }

  renderState(false)
}
