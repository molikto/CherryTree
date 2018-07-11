package web.view

case class ColorScheme(
  contentText: String = "#bbbb",
  contentBackground: String = "#282c34",
  contentSelectedBackground: String = "#333843",
  bottomBarBackground: String = "#1f2328",
  bottomBarText: String = "#969eac"
)

// TODO dynamic color scheme?
object ColorScheme {
  val default = ColorScheme()
}
