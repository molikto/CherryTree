import java.io.{File, PrintWriter}
import java.nio.charset.StandardCharsets
import java.util.Base64
import java.util.regex.Pattern


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
    val name0 = a.substring(k, j)
    val split = name0.split(" ")
    val name = split(0)
    println(split.mkString(" "))
    val color = split.find(_.startsWith("#")).getOrElse("#555c75")
    val svg = scala.io.Source.fromFile(s"fontawesome-free-5.2.0-desktop/svgs/$name.svg").getLines().mkString("\n")
      .replaceAllLiterally(s"<path ", s""""<path fill="$color" """)
    println(svg)
    val encoded = Base64.getEncoder.encodeToString(svg.getBytes(StandardCharsets.UTF_8))
    val replaceHandle = a.substring(i, j + 1)
    val toReplace = if (a.substring(j + 1).startsWith(" */")) {
      val s = a.substring(0, j).lastIndexOf("url(")
      val e = j + 4
      a.substring(s, e)
    } else {
      a.substring(i , j + 1)
    }
    a.replaceAllLiterally(toReplace, s"url(data:image/svg+xml;base64,$encoded ) /* $replaceHandle */")
  } else a
}).mkString("\n")

write(n, css)