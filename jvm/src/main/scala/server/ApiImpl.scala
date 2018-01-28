package server
import shared._
import shared.data._

object ApiImpl extends Api {

  override def authenticate(input: Authentication.Input): ClientState =
    ClientState(Document.empty, Node.Ref.root)
}
