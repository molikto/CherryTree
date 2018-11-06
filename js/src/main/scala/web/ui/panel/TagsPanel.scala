package web.ui.panel

import java.util.UUID

import client.Client
import command.{Command, Key}
import model.data.{Content, Text}
import org.scalajs.dom.raw.{HTMLElement, MouseEvent}
import scalatags.JsDom.all._
import web.ui.ContentListView
import web.ui.content.ContentView
import web.view.{DelayUpdate, UnselectableView, View}

import scala.collection.mutable.ArrayBuffer
import scala.scalajs.js

class TagsPanel(val client: Client, doc: () => View) extends UnselectableView with DelayUpdate {

  val tagsView = new ContentListView[model.data.Text.HashTag](tag => {
  }) {
    override def contentOf(t: Text.HashTag): HTMLElement = {
      val data = model.data.Content.Rich(model.data.Rich(Seq(t)))
      val view = ContentView.create(data, None).dom
      view.style.display = "inline-block"
      view
    }
  }
  dom = div(
    minWidth := "150px",
    width := "100%",
    height := "100%",
    whiteSpace := "pre-wrap",
    overflowY := "scroll",
    cls := "ct-scroll ct-panel",
    padding := "24px",
    tagsView
  ).render

  observe(client.stateUpdates.doOnNext { _ =>
    render()
  })

  private def state = client.state

  private var previousZoom: model.cursor.Node = null
  private var previousFocus: Option[UUID] = None

  override def renderDelayed(): Unit = render()

  private var previousTags: Object = null

  def render(): Unit = {
    val zoom = state.zoom
    val currentFocusTitleNode = state.focus.inits.map(a => state.node(a)).find(l => l.isHeading && !l.isH1)
    val focusSame = previousFocus == currentFocusTitleNode.map(_.uuid)
    val zoomSame = previousZoom == zoom
    if (zoomSame && focusSame) {
    } else {
      previousUpdateTime = -1L
    }
    if (checkShouldUpdate()) {
      previousZoom = zoom
      previousFocus = currentFocusTitleNode.map(_.uuid)
      val tags = state.node.allTags
      if (previousTags != tags) {
        previousTags = tags
        tagsView.update(tags.toSeq.sortBy(-_._2).map(_._1).toArray)
      }
    }
  }

  render()

}
