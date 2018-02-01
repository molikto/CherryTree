package server


object DevMain {
  def main(args: Array[String]): Unit = {
    new HttpServer().run()
  }
}
