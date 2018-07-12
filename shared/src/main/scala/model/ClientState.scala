package model


object ClientState {
  val empty = ClientState(model.data.Node.empty, Some(model.mode.Node.Content(Seq.empty, model.mode.Content.Insertion(0))))
}

case class ClientState(node: model.data.Node, mode: Option[model.mode.Node])
