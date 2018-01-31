package client

import shared.data._
import autowire._
import client.net.AutowireServer
import boopickle.Default._
import org.scalajs.dom

import scala.scalajs.js
import autowire._
import boopickle.Default._
import client.net.AutowireServer

import scala.util.{Failure, Success}
import scala.concurrent.ExecutionContext.Implicits.global
import org.scalajs.dom
import rxscalajs.{Observable, Subject}
import rxscalajs.subjects.BehaviorSubject
import shared._
import shared.data._

import scala.util.Success

class Client(initial: ClientState) {
  /**
    * out facing state
    */
  private val _state = BehaviorSubject[ClientState](initial)
  def state: Observable[ClientState] = _state
  /**
    * a ref of a server state
    */
  private var committed: ClientState = initial
  private var uncommitted: Seq[Transaction] = Seq.empty


  def update(changes: Transaction): Unit = {
    uncommitted = uncommitted ++ Seq(changes)
    //state = state.copy(state.document.copy())
//    api.change(state.authentication, changes).call().onComplete {
//      case Success(i) =>
//    }
  }
}
