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


    def cl(name: String) = ClientInitializer.init(server, Authentication.Token(name))

    'init - {
      Await.result(cl("client"), 1.seconds)
    }

    'client - {
      val client = Await.result(cl("client0"), 1.seconds)
      val client2 = Await.result(cl("client1"), 1.seconds)
      val clients = Seq(client, client2)

      def insertTop = Transaction(Seq(Change.Content.Insert(Node.PointRef(Node.Ref.root, 0), Random.nextLong() + " ")))
      def insertTop0(i: String) = Transaction(Seq(Change.Content.Insert(Node.PointRef(Node.Ref.root, 0), i + " ")))

      def waitAssertStateSyncBetweenClientsAndServer(): Unit = {
        val debug = false
        clients.foreach(c => {
          while (c.hasUncommited) {
            Thread.sleep(100)
          }
          c.sync()
        })
        Thread.sleep(100)
        if (debug) {
          println("Server doc " + serverDoc.root)
          clients.foreach(c => {
            println(c.debugCommited.authentication + " " + c.debugCommited.document.root)
          })
        }
        clients.foreach(c => {
          assert(c.debugCommited.document == serverDoc)
        })
      }

      'oneWaySync - {
        client.change(insertTop)
        waitAssertStateSyncBetweenClientsAndServer()
        client.change(insertTop)
        client.change(insertTop)
        client.change(insertTop)
        client.change(insertTop)
        waitAssertStateSyncBetweenClientsAndServer()
      }

      'randomTwoClientInsertionsSync - {
        val count = 1000
        for ((i, j) <- (0 until count).map(a => (a, Random.nextInt(2)))) {
          clients(j).change(insertTop0(s"${if (j == 0) "a" else "b"}$i"))
        }
        waitAssertStateSyncBetweenClientsAndServer()
        val str = serverDoc.root.content
        val ts = str.split(" ").filter(_.nonEmpty)
        assert(ts.size == count)
        def decreasing(a: Seq[Int]) = a.sliding(2, 1).forall(a => a.head > a(1))
        val as = ts.filter(_.startsWith("a")).map(_.drop(1).toInt)
        val bs = ts.filter(_.startsWith("b")).map(_.drop(1).toInt)
        assert(decreasing(as))
        assert(decreasing(bs))
        assert(as.toSet ++ bs.toSet == (0 until count).toSet)
      }
    }
  }
}
