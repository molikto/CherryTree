package shared.client

import shared._
import shared.data._
import utest._
import scala.concurrent.duration._

import scala.concurrent.Await
import scala.util.{Random, Try}


object ClientTests extends TestSuite  {

  val tests = Tests {
    val (server, api) = {
      val s = new TestServer()
      (s[Api], s.service)
    }

    def serverDoc = api.debugDocument
    def serverChanges = api.debugChanges


    def cl() = ClientInitializer.init(server, Authentication.Token(Random.nextString(10)))

    'init - {
      Await.result(cl(), 1.seconds)
      println("success!")
    }

    'client - {
      val client = Await.result(cl(), 1.seconds)

      val changeRootLine = Transaction(Seq(Change.Content.Insert(Node.PointRef(Node.Ref.root, 0), "what")))
      'single - {
        client.change(changeRootLine)
        Thread.sleep(100)
        println(client.state.get)
        println(serverDoc)
      }
    }
  }
}
