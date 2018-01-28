package shared

import shared.data._

trait Api {

  private var changes: Seq[Change] = Seq.empty

  def init(token: Authentication.Token): ClientState

  def change(snapshot: ClientStateSnapshot,
    changes: Seq[Change]): ClientStateUpdate
}
