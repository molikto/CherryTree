package server
import shared._
import shared.data._

import scala.collection.mutable

object ApiImpl extends Api {


  // don't have a data base now
  private val document = Document.empty
  private val clients: mutable.Map[Authentication.Token, ClientState] = mutable.Map.empty

  override def init(token: Authentication.Token): ClientState =
    ClientState(token, document, Node.Ref.root)

  override def change(snapshot: ClientStateSnapshot, changes: Seq[Change]): ClientStateUpdate = ???
}
