import java.io.{File, PrintWriter}
import java.nio.charset.StandardCharsets
import java.util.Base64


def write(string: String, file: File) = {
  file.getParentFile.mkdirs()
  file.createNewFile()
  val writer = new PrintWriter(file)
  writer.write(string)
  writer.close()
}

val css = new File("jvm/src/main/public/cherrytree.css")


val n = scala.io.Source.fromFile(css).getLines().map(a => {
  val i = a.indexOf("fas(")
  if (i >= 0) {
    val k = i +  "fas(".length
    val j = a.indexOf(")", i)
    val name = a.substring(k, j)
    val svg = scala.io.Source.fromFile(s"fontawesome-free-5.2.0-desktop/svgs/$name.svg").getLines().mkString("\n").replaceAllLiterally("<path ", "<path fill=\"#555c75\" ")
    val encoded = Base64.getEncoder.encodeToString(svg.getBytes(StandardCharsets.UTF_8))
    a.replaceAllLiterally(a.substring(i, j + 1), s"url(data:image/svg+xml;base64,$encoded)")
  } else a
}).mkString("\n")

write(n, css)