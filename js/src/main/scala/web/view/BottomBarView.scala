package web.view

import client.Client
import command.Key.KeySeq
import command.{CommandStatus, Key}
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

  private def divider() = span(" | ", color := theme.disalbedInfo)


  dom = div(
    width := "100%",
    paddingTop := "1px",
    paddingLeft := "8px",
    fontSize := "14px",
    alignSelf := "flex-end",
    `class` := "unselectable",
    height := size,
    backgroundColor := theme.bottomBarBackground,
    flexDirection := "row",
    color := theme.bottomBarText,
    mode,
    divider(),
    commandStatus,
    divider(),
    debugErrorInfo
  ).render

  observe(client.commandStatus.doOnNext(c => {
    val (text, color) = c match {
      case CommandStatus.Empty => (EmptyStr, theme.disalbedInfo)
      case CommandStatus.InputtingCount(a: String) => (a, null)
      case CommandStatus.WaitingForConfirm(count: String, k: KeySeq) => (Seq(count, renderKeySeq(k)).filter(_.nonEmpty).mkString(" "), null)
      case CommandStatus.WaitingForChar(count: String, k: KeySeq) => (Seq(count, renderKeySeq(k)).filter(_.nonEmpty).mkString(" "), null)
      case CommandStatus.LastPerformed(count: String, k: KeySeq, char: Option[Unicode]) =>
        (Seq(count, renderKeySeq(k), char.map(_.toString).getOrElse("")).filter(_.nonEmpty).mkString(" "), theme.disalbedInfo)
      case CommandStatus.LastNotFound(count: String, k: KeySeq) =>
        (Seq(count, renderKeySeq(k)).filter(_.nonEmpty).mkString(" "), theme.littleError)
    }
    commandStatus.textContent = text
    commandStatus.style.color = color
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
              "CODE BLOCK"
            case model.mode.Content.CodeInside =>
              "CODE"
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
}
