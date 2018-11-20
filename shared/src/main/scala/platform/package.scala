import model.data
import register.Registerable


package object platform {


  var isMac: Boolean = false
  var parseFromCommonMarkMarkdown: String => data.Node = null
  var parseFromHtml: String => Registerable = null

  var formatDate: Long => String = null

}
