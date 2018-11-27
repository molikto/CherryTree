package web.ui.panel

import java.util.UUID

import client.Client
import model.data.{Content, Text}
import org.scalajs.dom.raw.{HTMLElement, MouseEvent}
import scalatags.JsDom.all._
import web.ui.ContentListView
import web.ui.content.{ContentView, ContentViewCreator}
import web.ui.dialog.QuickSearchDialog
import web.ui.doc.{DocumentView, LaTeXMacroCache}
import web.view.{DelayUpdate, UnselectableView, View}

import scala.scalajs.js

class TagsPanel(val client: Client, doc: => View, quickSearch: => QuickSearchDialog, override val latexMacroCache: LaTeXMacroCache) extends UnselectableView with DelayUpdate with ContentViewCreator {

  val tagsView = new ContentListView[model.data.Text.HashTag](tag => {
  }) {
    override def contentOf(t: Text.HashTag): HTMLElement = {
      val data = model.data.Content.Rich(model.data.Rich(t.content))
      val view = contentViewCreate(data, None).dom
      view.classList.add("ct-flat-selectable")
      val li: js.Function1[MouseEvent, _] =  (mouse: MouseEvent) => {
        quickSearch.showWithTag(t)
      }
      view.onclick = li
      view
    }
  }
  dom = div(
    minWidth := "150px",
    width := "100%",
    height := "100%",
    whiteSpace := "pre-wrap",
    overflowY := "scroll",
    cls := "ct-scroll ct-panel ct-document-style",
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
    val currentFocusTitleNode = state.focus.inits.map(a => state.node(a)).find(l => l.isHeading && !l.isFolder)
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
