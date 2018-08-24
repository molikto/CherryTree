package web.view

import api.ServerStatus
import client.Client
import command.Key.KeySeq
import command.Key
import model.data.{Content, Unicode}
import model.{cursor, data, mode}
import monix.execution.{Ack, Scheduler}
import monix.reactive.observers.Subscriber
import org.scalajs.dom.window
import org.scalajs.dom.document
import org.scalajs.dom
import org.scalajs.dom.html
import org.scalajs.dom.html.{Div, Span}
import org.scalajs.dom.raw._

import scala.collection.mutable.ArrayBuffer
import scala.scalajs.js
import scalatags.JsDom.all._
import monix.execution.Scheduler.Implicits.global

import scala.concurrent.Future
import scala.util.Random

class BottomBarView(val client: Client) extends UnselectableView  {


  val size = "24px"


  private val mode = span("").render
  private val commandStatus = span("").render

  private val debugVersionInfo = span(marginLeft := "12px", "0").render

  private val debugErrorInfo = span(`class` := "ct-error-color", marginLeft := "12px", "").render

  private val connection = span(alignSelf.flexEnd).render

  private def divider() = span(" | ", `class` := "ct-hint-color")


  dom = div(
    width := "100%",
    `class` := "ct-panel",
    paddingTop := "1px",
    paddingLeft := "8px",
    paddingRight := "8px",
    fontSize := "14px",
    alignSelf := "flex-end",
    height := size,
    flexDirection := "row",
    connection,
    divider(),
    mode,
    divider(),
    commandStatus,
    divider(),
    debugErrorInfo
  ).render

  observe(client.sourceEditorCommands.doOnNext(str => {
//    commandStatus.textContent = if (str.isEmpty) EmptyStr else str
//    commandStatus.className = "ct-hint-color"
  }))

  observe(client.commandBufferUpdates.doOnNext(c => {
    var isCompleted = false
    var isError = false
    val ts = c.map {
      case command.Part.IdentifiedCommand(key, c, _) =>
        key match {
          case Some(ss) => Key.toString(ss)
          case None if c.keys.nonEmpty => Key.toString(c.keys.head)
          case _ => "(command)"
        }
      case command.Part.UnidentifiedCommand(key, _) => Key.toString(key)
      case command.Part.UnknownCommand(key) =>
        isError = true
        Key.toString(key)
      case command.Part.Count(c) => c.toString
      case command.Part.Char(c) => c.str
      case command.Part.CompleteMark =>
        isCompleted = true
        ""
      case command.Part.UnknownPatternMark =>
        isError = true
        ""
    }

    val cs = ts.filter(!_.isEmpty).mkString(" ")
    if (cs.isEmpty) {
      commandStatus.textContent = EmptyStr
      commandStatus.className = "ct-hint-color"
    } else {
      commandStatus.textContent = cs
      if (isCompleted) {
        commandStatus.className = "ct-hint-color"
      } else if (isError) {
        commandStatus.className = "ct-error-color"
      } else {
        commandStatus.className = ""
      }
    }
  }))

  private def updateModeIndicator(): Unit = {
    val text = client.state.mode match {
      case None =>
        ""
      case Some(mm) => mm match {
        case model.mode.Node.Content(at, aa) =>
          def oneLevel(a: model.mode.Content): String = {
            a match {
              case model.mode.Content.RichInsert(_) =>
                "INSERT"
              case model.mode.Content.RichVisual(_, _) =>
                "VISUAL"
              case model.mode.Content.RichNormal(_) =>
                "NORMAL"
              case model.mode.Content.CodeNormal =>
                "CODE"
              case model.mode.Content.CodeInside(mode, int) =>
                s"CODE : ${mode.toUpperCase}"
              case model.mode.Content.RichCodeSubMode(range, code, before) =>
                s"${oneLevel(before)}: ${code.mode.toUpperCase()}"
              case model.mode.Content.RichAttributeSubMode(range, before) =>
                oneLevel(before)
            }
          }
          oneLevel(aa)
        case v@model.mode.Node.Visual(_, _) =>
          "NODE VISUAL"
      }
    }
    if (text == "") {
      mode.textContent = EmptyStr
      mode.className = "ct-hint-color"
    } else {
      mode.textContent = text
      mode.className = ""
    }
  }

  updateModeIndicator()

  observe(client.stateUpdates.doOnNext(update => {
    updateModeIndicator()
  }))


  event(window, "error", (e: Event) => {
    window.console.log(e)
    debugErrorInfo.textContent = e.asInstanceOf[js.Dynamic].message.asInstanceOf[String]
  })

  observe(client.errors.doOnNext {
    case Some(e) => debugErrorInfo.textContent = e.getMessage
    case _ =>
  })

  observe(client.connection.doOnNext {
    case ServerStatus(count, offline, tempOffline) =>
      if (offline) {
        connection.textContent = "offline"
        connection.className = "ct-error-color"
      } else if (tempOffline) {
        connection.textContent = "offline"
        connection.className = "ct-hint-color"
      } else {
        connection.textContent = s"$count user${if (count == 1) "" else "s"} online"
        connection.className = "ct-hint-color"
      }
  })
}
