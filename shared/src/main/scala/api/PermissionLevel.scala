package api

object PermissionLevel {
  val Read = 5
  val Comment = 30
  val Edit = 50
  val Admin = 80
  val Owner = 100

  def toName(i: Int): String = {
    if (i >= Owner) {
      "Owner"
    } else if (i >= Admin) {
      "Admin"
    } else if (i >= Edit) {
      "Editor"
    } else if (i >= Comment) {
      "Commentor"
    } else if (i >= Read) {
      "Read-only"
    } else {
      ""
    }
  }
}
