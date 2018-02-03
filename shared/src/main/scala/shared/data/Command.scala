package shared.data





sealed trait Command {

}

object Command {

  object Node {
    object Delete extends Command
    object Append
  }
  object Content {
    object
  }
}
