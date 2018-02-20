package shared

object gen extends shared.ot.gen.Gen {

  /**
    * document
    */

  val content = coproduct("content",
    "text" -> string
  )

  val node = recursive(a => product("node",
    "content" -> content,
    "childs" -> seq(a)
  ))


  val document = product("document",
    "root" -> node
  )


  /**
    * node
    */

  val key_setting = product("key setting",
    "name" -> string
  )

  val settings = product("settings",
    "keys" -> set(key_setting)
  )

}
