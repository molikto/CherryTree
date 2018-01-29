package shared.data

import utest._


object NodeTests extends TestSuite {

  val tests = Tests {
    'destructRelative - {
      def test(a: Seq[Int], b: Seq[Int], common: Seq[Int], ap: Seq[Int], bp: Seq[Int]) = {
        assert(Node.Ref.destructRelative(Node.Ref(a), Node.Ref(b)) == (common, ap, bp))
      }
      'test1 - {
        test(Seq(0, 1, 2, 3), Seq(0, 1), Seq(0, 1), Seq(2, 3), Seq.empty)
      }
      'test2 - {
        test(Seq(0, 1, 2, 3), Seq(0, 1, 5, 6), Seq(0, 1), Seq(2, 3), Seq(5, 6))
      }
      'test3 - {
        test(Seq(0, 1, 2, 3), Seq(5, 1, 5, 6), Seq.empty, Seq(0, 1, 2, 3), Seq(5, 1, 5, 6))
      }
    }
    'transformDeleted - {
      def test(a: Seq[Int], b: Seq[Int], c: Option[Seq[Int]]) = {
        assert(Node.Ref.transformAfterDeleted(
          Node.Ref.root.withChilds(a.toArray :_*),
          Node.Ref.root.withChilds(b.toArray: _*))
          == c.map(it => Node.Ref.root.withChilds(it.toArray: _*)))
      }
      'basics - {
        test(Seq(0, 2), Seq(0, 2, 3), None)
        test(Seq(0, 2), Seq(0, 2), None)
        test(Seq(0, 2), Seq(0, 1), Some(Seq(0, 1)))
        test(Seq(0, 2), Seq(0, 3), Some(Seq(0, 2)))
        test(Seq(0, 2), Seq(1), Some(Seq(1)))
        test(Seq(0, 2), Seq(0, 3, 4), Some(Seq(0, 2, 4)))
        test(Seq(0, 2), Seq(1, 3, 4), Some(Seq(1, 3, 4)))
      }
    }
    'transformInserted - {
      def test(a: Seq[Int], b: Seq[Int], c: Seq[Int]) = {
        assert(Node.Ref.transformAfterInserted(
          Node.Ref.root.withChilds(a.toArray :_*),
          Node.Ref.root.withChilds(b.toArray: _*))
          == Node.Ref.root.withChilds(c.toArray: _*))
      }
      'basics - {
        test(Seq(0, 2), Seq(0, 3), Seq(0, 4))
        test(Seq(0, 3), Seq(0, 2), Seq(0, 2))
        test(Seq(0), Seq(0, 1), Seq(1, 1))
        test(Seq(0), Seq(1, 1), Seq(2, 1))
        test(Seq(1), Seq(0, 1), Seq(0, 1))
        test(Seq(0, 2, 0), Seq(0, 3), Seq(0, 3))
      }
    }
  }
}
