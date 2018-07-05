package model.cursor

import model._
import utest._

object CursorTests extends TestSuite {

  val tests = Tests {
    'node - {
      'sameInsertWillMoveBack - {
        assert(Node.transformAfterInserted(Seq(1,2,3), 4, Seq(1,2,3)) == Seq(1,2,7))
      }
      'sameDeeperInsertWillMoveBack - {
        assert(Node.transformAfterInserted(Seq(1,2,3), 4, Seq(1,2,3,5)) == Seq(1,2,7,5))
      }
      'laterInsertWillMoveBack - {
        assert(Node.transformAfterInserted(Seq(1,2,3), 4, Seq(1,2,4)) == Seq(1,2,8))
      }
      'laterDeeperInsertWillMoveBack - {
        assert(Node.transformAfterInserted(Seq(1,2,3), 4, Seq(1,2,4,5)) == Seq(1,2,8,5))
      }
      'newerInsertNotAffected - {
        assert(Node.transformAfterInserted(Seq(1,2,3), 4, Seq(1,2,2)) == Seq(1,2,2))
      }
      'previousLevelInsertNotAffected- {
        assert(Node.transformAfterInserted(Seq(1,2,3), 4, Seq(1,2)) == Seq(1,2))
      }
      'notRelevantNotAffected - {
        assert(Node.transformAfterInserted(Seq(1,2,3), 4, Seq(1,4,7)) == Seq(1,4,7))
      }
      'parentPointNotAffected - {
        assert(Node.transformAfterInserted(Seq(1,2,3), 4, Seq(1,2)) == Seq(1,2))
      }
    }

 }
}
