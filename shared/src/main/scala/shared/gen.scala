package shared

object gen extends shared.ot.gen.Gen("shared.data0") {

  /**
    * document
    */

  val Content = coproduct("Content",
    "text" -> string
  )

  val Node = recursive(a => product("Node",
    "content" -> Content,
    "childs" -> seq(a)
  ))


  val Document = product("Document",
    "root" -> Node
  )


  /**
    * node
    */

  val KeySetting = product("KeySetting",
    "name" -> string
  )

  val Settings = product("Settings",
    "keys" -> set(KeySetting)
  )

}
