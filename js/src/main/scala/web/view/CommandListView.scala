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

class CommandListView(parent: HTMLElement, val client: Client) extends View {


  dom = div(flex := "0 0 auto",height := "100%", minWidth := "150px", background := theme.bottomBarBackground).render
  parent.appendChild(dom)

  observe(client.stateUpdates.doOnNext(_ => {
    update()
  }))

  update()

  def update(): Unit = {
    removeAllChild(dom)
  }

}
