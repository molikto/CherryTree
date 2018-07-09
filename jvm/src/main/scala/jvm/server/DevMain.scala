package jvm.server

object DevMain {
  def main(args: Array[String]): Unit = {
    new WebServer().run()
  }
}
