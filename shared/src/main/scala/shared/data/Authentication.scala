package shared.data



object Authentication {
  case class Pass(uid: String)
  case class Token(stuff: String)
}
