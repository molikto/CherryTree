package client

import shared.data._
import autowire._
import client.net.AutowireServer
import boopickle.Default._

import scala.util.Success

class ClientModel(initial: ClientState,
  val onChange: ClientState => Unit) {
  /**
    * out facing state
    */
  private var _state = initial
  def state: ClientState = _state
  private def state_=(s: ClientState): Unit = {
    _state = s
    onChange(s)
  }

  /**
    * committed and uncommitted stuff
    */
  private var committedState: ClientState = initial

  private var uncommittedChanges: List[Change] = List.empty


  def update(changes: List[Change]): Unit = {
    uncommittedChanges = uncommittedChanges ++ changes
    state = state.copy(document = Change.apply(state.document, changes))
    api.change(state.authentication, changes).call().onComplete {
      case Success(i) =>
    }
  }
}
