package web.view

case class ColorScheme(
  contentText: String = "#abb2bf",
  contentBackground: String = "#282c34",
  bottomBarBackground: String = "#1f2328",
  bottomBarText: String = "#969eac"
)

// TODO dynamic color scheme?
object ColorScheme {
  val default = ColorScheme()
}
