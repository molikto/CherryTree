package shared

import shared.data._

trait Api {

  def init(token: Authentication.Token): ClientState

  def change(snapshot: ClientStateSnapshot,
    changes: Seq[Transaction]): ClientStateUpdate
}
