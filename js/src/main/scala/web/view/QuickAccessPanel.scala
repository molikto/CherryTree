package web.view


import client.Client
import command.Key
import _root_.doc.DocTransaction
import monix.execution.Ack
import scalatags.JsDom.all._
import org.scalajs.dom._
import util.QuickDiff
import web.view.content.{StaticDiffContentListView, StaticDiffTocView}

import scala.concurrent.Future

class QuickAccessPanel(client: Client) extends UnselectableView {

  private def onClick(uuid: String): Unit = {
    client.state.lookup(uuid) match {
      case Some(cur) =>
        client.localChange(client.state.zoomTo(cur, client.enableModal))
      case _ =>
    }
  }

  private val parentsView = new StaticDiffContentListView(onClick)
  parentsView.addHeader(div(`class` := "ct-section-label", "go up").render)
  private val tocView = new StaticDiffTocView(onClick)
  tocView.addHeader(div(`class` := "ct-section-label", "table of content").render)

  dom = div(
    minWidth := "150px",
    width := "100%",
    height := "100%",
    overflowY := "scroll",
    `class` := "ct-scroll ct-panel ct-document-style ",
    fontSize := "16px",
    padding := "24px",
    parentsView,
    div(height := "24px"),
    tocView
  ).render


  private def state = client.state

  observe(client.stateUpdates.doOnNext(update => {
    renderState(update.from.isEmpty)
  }))

  private var previousUpdateTime = 0L
  private var scheduledUpdate: Int = -1
  private var previousZoom: model.cursor.Node = null
  private var previousFocus: Option[String] = None
  private var previousNearestHeading: Option[String] = None

  def renderState(emptyDataChange: Boolean): Unit = {
    val zoom = state.zoom
    val currentFocusTitleNode = state.focus.inits.map(a => state.node(a)).find(l => l.isHeading)
    val focusSame = previousFocus == currentFocusTitleNode.map(_.uuid)
    if (previousZoom == zoom && focusSame) {
      if (emptyDataChange) {
        return
      }
    } else {
      previousUpdateTime = -1L
    }
    var t = System.currentTimeMillis()
    if (t - previousUpdateTime < 1000) {
      if (scheduledUpdate == -1) {
        window.setTimeout(() => {
          scheduledUpdate = -1
          renderState(false)
        }, previousUpdateTime + 1100 - t)
      }
      return
    }
    previousUpdateTime = t
    if (scheduledUpdate != -1) {
      window.clearTimeout(scheduledUpdate)
      scheduledUpdate = -1
    }
    previousZoom = zoom
    val nearestHeadingCur = zoom.inits.find(l => state.node(l).isH1)
    val nearestHeading = nearestHeadingCur.map(cur => state.node(cur))
    val showParentsOf = nearestHeadingCur.map(_ :+ 0).getOrElse(zoom)
    parentsView.update(showParentsOf.indices.map(a => state.node(zoom.take(a))))
    if (emptyDataChange && previousNearestHeading == nearestHeading.map(_.uuid)) {
      // no toc changes
    } else {
      if (previousNearestHeading.isDefined != nearestHeading.isDefined) {
        tocView.dom.style.display = if (nearestHeading.isDefined) "auto" else "none"
      }
      previousNearestHeading = nearestHeading.map(_.uuid)
      tocView.update(nearestHeading.map(_.childs).getOrElse(Seq.empty))
    }
    if (!focusSame) {
      previousFocus = currentFocusTitleNode.map(_.uuid)
      tocView.updateFocus(previousFocus, dom)
    }
    t = System.currentTimeMillis() - t
    if (t > 0 && model.debug_view) {
      println(s"quick access updated in $t")
    }
  }

  renderState(false)
}
