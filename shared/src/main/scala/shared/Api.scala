package shared

import shared.data._

trait Api {
  def authenticate(input: Authentication.Input): ClientState

  def change(token: Authentication.Token, changes: List[Change]): ClientStateUpdate
}
