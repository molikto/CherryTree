package shared.client

import shared._
import shared.test._
import shared.data._
import shared.server.CherryTreeServer
import utest._

import scala.concurrent.duration._
import scala.concurrent.Await
import scala.util.{Random, Try}


object ClientTests extends TestSuite  {

  val tests = Tests {
    val server = new CherryTreeServer()
    val api = {
      val s = new TestApiAdapter(server)
      s[Api]
    }

    def serverDoc = server.debugDocument
    def serverChanges = server.debugChanges


    def cl(name: String) = ClientInitializer.init(api, Authentication.Token(name))

    'init - {
      Await.result(cl("client"), 1.seconds)
    }

    'client - {
      val clients = (0 until 24).map(i => Await.result(cl("client" + i), 1.seconds))
      val client = clients.head

      def insertTop = Transaction(Seq(Change.Content.Insert(Node.PointRef(Node.Ref.root, 0), Random.nextLong() + " ")))
      def insertTop0(i: String) =
        Transaction(Seq(Change.Content.Insert(Node.PointRef(Node.Ref.root, 0), i + " ")))

      def waitAssertStateSyncBetweenClientsAndServer(): Unit = {
        val debug = false
        while (clients.exists(a => a.hasUncommited)) {
          Thread.sleep(100)
        }
        clients.foreach(_.sync())
        while (clients.exists(a => a.updating)) {
          Thread.sleep(100)
        }
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

      'randomTwoClientTopInsertionsSync - {
        val count = 100
        for ((i, j) <- (0 until count).map(a => (a, Random.nextInt(clients.size)))) {
          clients(j).change(insertTop0(s"${('a' + j).toChar}$i"))
        }
        waitAssertStateSyncBetweenClientsAndServer()
        val str = serverDoc.root.content
        val ts = str.split(" ").filter(_.nonEmpty)
        assert(ts.size == count)
        def decreasing(a: Seq[Int]) = a.sliding(2, 1).forall(a => a.size < 2 || a.head > a(1))
        val vs = clients.indices.map(i => ts.filter(_.startsWith(('a' + i).toChar.toString)).map(_.drop(1).toInt).toSeq)
        assert(vs.forall(decreasing))
        assert(vs.flatten.toSet == (0 until count).toSet)
      }
      'randomSingleChangeSync - {
        val count = 1000
        for ((_, j) <- (0 until count).map(a => (a, Random.nextInt(clients.size)))) {
          // sadly our tests is not one thread
          clients(j).synchronized {
            clients(j).change(randomSingleChangeTransaction(clients(j).root.get))
          }
        }
        waitAssertStateSyncBetweenClientsAndServer()
      }
    }
  }
}
