package web.view

import client.Client
import command.Key.KeySeq
import command.{Key}
import model.data.{Content, Unicode}
import model.{cursor, data, mode}
import monix.execution.{Ack, Scheduler}
import monix.reactive.observers.Subscriber
import org.scalajs.dom.window
import org.scalajs.dom.document
import org.scalajs.dom
import org.scalajs.dom.html
import org.scalajs.dom.html.Div
import org.scalajs.dom.raw._

import scala.collection.mutable.ArrayBuffer
import scala.scalajs.js
import scalatags.JsDom.all._
import monix.execution.Scheduler.Implicits.global

import scala.concurrent.Future
import scala.util.Random

class BottomBarView(val client: Client) extends View {


  val size = "24px"


  private val mode = span("").render
  private val commandStatus = span("").render

  private val debugVersionInfo = span(marginLeft := "12px", "0").render

  private val debugErrorInfo = span(color := "red", marginLeft := "12px", "").render

  private val connection = span(alignSelf.flexEnd).render

  private def divider() = span(" | ", color := theme.disalbedInfo)


  dom = div(
    width := "100%",
    paddingTop := "1px",
    paddingLeft := "8px",
    paddingRight := "8px",
    fontSize := "14px",
    alignSelf := "flex-end",
    height := size,
    backgroundColor := theme.bottomBarBackground,
    flexDirection := "row",
    color := theme.bottomBarText,
    connection,
    divider(),
    mode,
    divider(),
    commandStatus,
    divider(),
    debugErrorInfo
  ).render

  observe(client.commandBufferUpdates.doOnNext(c => {
    var isCompleted = false
    var isError = false
    val ts = c.map {
      case command.Part.IdentifiedCommand(key, c, _) =>
        key match {
          case Some(ss) => renderKeySeq(ss)
          case None if c.keys.nonEmpty => renderKeySeq(c.keys.head)
          case _ => "(command)"
        }
      case command.Part.UnidentifiedCommand(key, _) => renderKeySeq(key)
      case command.Part.UnknownCommand(key) =>
        isError = true
        renderKeySeq(key)
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
      commandStatus.style.color = theme.disalbedInfo
    } else {
      commandStatus.textContent = cs
      commandStatus.style.color = if (isCompleted) theme.disalbedInfo else if (isError) theme.littleError else null
    }
  }))

  private def updateModeIndicator(): Unit = {
    val text = client.state.mode match {
      case None =>
        ""
      case Some(mm) => mm match {
        case model.mode.Node.Content(at, aa) =>
          aa match {
            case model.mode.Content.RichInsert(_) =>
              "INSERT"
            case model.mode.Content.RichVisual(_, _) =>
              "VISUAL"
            case model.mode.Content.RichNormal(_) =>
              "NORMAL"
            case model.mode.Content.CodeNormal =>
              "CODE"
            case model.mode.Content.CodeInside =>
              "CODE EDIT"
          }
        case v@model.mode.Node.Visual(_, _) =>
          "NODE VISUAL"
      }
    }
    if (text == "") {
      mode.textContent = EmptyStr
      mode.style.color = theme.disalbedInfo
    } else {
      mode.textContent = text
      mode.style.color = null
    }
  }

  updateModeIndicator()

  observe(client.stateUpdates.doOnNext(update => {
    updateModeIndicator()
  }))

  observe(client.errors.doOnNext {
    case Some(e) => debugErrorInfo.textContent = e.getMessage
    case _ =>
  })

  observe(client.connection.doOnNext {
    case Some(e) =>
      connection.textContent = s"${e.online} user${if (e.online == 1) "" else "s"} online"
      connection.style.color = theme.disalbedInfo
    case _ =>
      connection.textContent = "offline"
      connection.style.color = theme.littleError
  })
}
