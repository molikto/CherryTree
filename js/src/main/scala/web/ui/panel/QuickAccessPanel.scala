package web.ui.panel

import java.util.UUID

import client.Client
import org.scalajs.dom._
import org.scalajs.dom.raw.{HTMLElement, HTMLOptionElement, HTMLSelectElement}
import scalatags.JsDom.all._
import web.WebApi
import web.view.{DelayUpdate, UnselectableView, View}

import scala.util.Try

class QuickAccessPanel(client: Client, doc: () => View) extends UnselectableView with DelayUpdate {

  private var previousZoom: model.cursor.Node = null
  private var previousFocus: Option[UUID] = None
  private var previousNearestHeading: Option[UUID] = None

  private def onClick(uuid: UUID): Unit = {
    client.state.lookup(uuid) match {
      case Some(cur) =>
        client.localChange(client.state.goTo(cur, client))
        doc().focus()
      case _ =>
    }
  }


  private def onDoubleClick(uuid: UUID): Unit = {
    val cur = client.state.lookup(uuid)
    cur match {
      case Some(cur) =>
        client.localChange(client.state.goTo(cur, client, true))
        doc().focus()
      case _ =>
    }
  }



  private val parentsView = new StaticDiffContentListView(onClick, onDoubleClick)
  parentsView.addHeader(div(cls := "ct-section-label", "go up").render)
  private val tocView = new StaticDiffTocView(onClick, onDoubleClick, 1)

  private var hideLevel: Int = Try {WebApi.localStorage.get("hide_level").get.toInt }.getOrElse(3)

  private val selectView: HTMLSelectElement = select(
    height := "24px",
    cls := "ct-select",
    flex := "0 1 auto",
    alignSelf := "right",
    (3 to 6).map(a => option(s"hide heading $a", value := a.toString)),
    onfocus := { e: Event =>
      doc().focus()
    },
    onchange := { e: Event => {
      val level = e.target.asInstanceOf[HTMLOptionElement].value.toInt
      if (hideLevel != level) {
        hideLevel = level
        WebApi.localStorage.set("hide_level", hideLevel.toString)
        tocView.updateFocus(previousFocus, hideLevel, dom)
      }
      doc().focus()
    }},
  ).render

  selectView.selectedIndex = hideLevel - 3

  tocView.addHeader(div(
    alignContent := "middle",
    display := "flex",
    justifyContent := "space-between",
    flexDirection := "row",
    cls := "ct-section-label",
    "contents",
    selectView
  ).render)

  dom = div(
    minWidth := "150px",
    width := "100%",
    height := "100%",
    overflowY := "scroll",
    cls := "ct-scroll ct-panel ct-document-style ",
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



  override def renderDelayed(): Unit = renderState(false)

  def renderState(emptyDataChange: Boolean): Unit = {
    val zoom = state.zoom
    val currentFocusTitleNode = state.focus.inits.map(a => state.node(a)).find(l => l.isHeading && !l.isH1)
    val focusSame = previousFocus == currentFocusTitleNode.map(_.uuid)
    val zoomSame = previousZoom == zoom
    if (zoomSame && focusSame) {
      if (emptyDataChange) {
        return
      }
    } else {
      previousUpdateTime = -1L
    }
    if (checkShouldUpdate()) {
      previousZoom = zoom
      val nearestHeadingCur = zoom.inits.find(l => state.node(l).isH1)
      val nearestHeading = nearestHeadingCur.map(cur => state.node(cur))
      parentsView.update(zoom.indices.map(a => state.node(zoom.take(a))))
      var tocChanged = false
      if (emptyDataChange && previousNearestHeading == nearestHeading.map(_.uuid)) {
        // no toc changes
      } else {
        tocChanged = true
        if (previousNearestHeading.isDefined != nearestHeading.isDefined) {
          tocView.dom.style.display = if (nearestHeading.isDefined) "auto" else "none"
        }
        previousNearestHeading = nearestHeading.map(_.uuid)
        tocView.update(nearestHeading.map(_.childs).getOrElse(Seq.empty))
      }
      if (!focusSame || tocChanged) {
        previousFocus = currentFocusTitleNode.map(_.uuid)
        tocView.updateFocus(previousFocus, hideLevel, dom)
      }
    }
  }

  renderState(false)


}
