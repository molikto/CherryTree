package jvm.server

object Main {
  def main(args: Array[String]): Unit = {
    new WebServer().run()
  }
}
