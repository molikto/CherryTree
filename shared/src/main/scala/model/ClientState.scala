package model


object ClientState {
  val empty = ClientState(model.data.Node.empty, Some(model.mode.Node.Content(Seq.empty, model.mode.Content.Insert(0))))
}

case class ClientState(node: model.data.Node, mode: Option[model.mode.Node]) {
  def isInserting: Boolean = mode match {
    case Some(model.mode.Node.Content(_, model.mode.Content.Insert(_))) => true
    case _ => false
  }
}
