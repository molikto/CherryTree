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
    'transformInserted - {
      def test(a: Seq[Int], b: Seq[Int], c: Seq[Int]) = {
        assert(Node.Ref.transformAfterInserted(
          Node.Ref.root.withChilds(a.toArray :_*),
          Node.Ref.root.withChilds(b.toArray: _*))
          == Node.Ref.root.withChilds(c.toArray: _*))
      }
      'test1 - {
        test(Seq(0, 2), Seq(0, 3), Seq(0, 4))
      }

      'test2 - {
        test(Seq(0, 3), Seq(0, 2), Seq(0, 2))
      }
      'test3 - {
        test(Seq(0), Seq(0, 1), Seq(1, 1))
      }
      'test4 - {
        test(Seq(1), Seq(0, 1), Seq(0, 1))
      }
    }
  }
}
