package web.ui.panel

import client.Client
import org.scalajs.dom._
import org.scalajs.dom.raw.{HTMLElement, HTMLOptionElement, HTMLSelectElement}
import scalatags.JsDom.all._
import web.view.{UnselectableView, View}

import scala.util.Try

class QuickAccessPanel(client: Client, doc: () => View) extends UnselectableView {

  private var previousUpdateTime = 0L
  private var scheduledUpdate: Int = -1
  private var previousZoom: model.cursor.Node = null
  private var previousFocus: Option[String] = None
  private var previousNearestHeading: Option[String] = None

  private def onClick(uuid: String): Unit = {
    client.state.lookup(uuid) match {
      case Some(cur) =>
        client.localChange(client.state.zoomTo(cur, client.enableModal))
        doc().focus()
      case _ =>
    }
  }

  private val parentsView = new StaticDiffContentListView(onClick)
  parentsView.addHeader(div(`class` := "ct-section-label", "go up").render)
  private val tocView = new StaticDiffTocView(onClick, 1)

  private var hideLevel: Int = Try {_root_.client.localStorage.get("hide_level").get.toInt }.getOrElse(3)

  private val selectView: HTMLSelectElement = select(
    height := "24px",
    `class` := "ct-select",
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
        _root_.client.localStorage.set("hide_level", hideLevel.toString)
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
    `class` := "ct-section-label",
    "contents",
    selectView
  ).render)

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



  /**
    * will also remove from parent
    * ALSO make sure you destroy child dom attachments!!!
    */
  override def destroy(): Unit = {
    if (scheduledUpdate != -1) {
      window.clearTimeout(scheduledUpdate)
    }
    super.destroy()
  }

  def renderState(emptyDataChange: Boolean): Unit = {
    val zoom = state.zoom
    val currentFocusTitleNode = state.focus.inits.map(a => state.node(a)).find(l => l.isHeading && !l.isH1)
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
        scheduledUpdate = window.setTimeout(() => {
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
    parentsView.update(zoom.indices.map(a => state.node(zoom.take(a))))
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
      tocView.updateFocus(previousFocus, hideLevel, dom)
    }
    t = System.currentTimeMillis() - t
    if (t > 0 && model.debug_view) {
      println(s"quick access updated in $t")
    }
  }

  renderState(false)
}
