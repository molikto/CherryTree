package util.diff

import util.diff.OperationType._

import scala.collection.mutable.ArrayBuffer

case class Diff(original: Seq[String], modified: Seq[String], diffs: List[Operation]) {

  /**
   * The unaltered diff result
   */
  override def toString: String = {
    diffs.mkString
  }

  /**
   * Create a nice HTML report of the diff
   */
  def html: String = {
    Diff.clean(diffs).foldLeft("") { (html, diff) =>
      val text = diff.text.mkString("").replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;").replace("\n", "<br>")
      val tag = diff.op match {
        case Insert => s"<ins>$text</ins>"
        case Delete => s"<del>$text</del>"
        case Equals => s"<span>$text</span>"
      }
      html + tag
    }
  }

  /**
   * Convert the diff into a more human-readable format
   */
  def humanized: String = {
    Diff.clean(diffs).foldLeft("") { (res, diff) =>
      val text = diff.op match {
        case Insert => s"+[${diff.text.mkString("")}]"
        case Delete => s"-[${diff.text.mkString("")}]"
        case Equals => diff.text.mkString("")
      }
      res + text
    }
  }
}

object Diff {
  // Cost of an empty edit operation in terms of edit characters
  private val MAX_EDIT_COST = 4

  /**
   * Creates a new Diff
   * @param a First string
   * @param b Second string
   * @return Diff
   */
  def create(a: Seq[String], b: Seq[String]): Diff = {
    import scala.util.control.Breaks._

    var (original: Seq[String], modified: Seq[String]) = (a, b)
    var subsequence: Seq[String] = lcs(original, modified)
    var result = List.empty[Operation]

    while (subsequence.nonEmpty) {
      val sFirst = subsequence.head
      subsequence = subsequence.drop(1)

      breakable { while (modified.nonEmpty) {
        val mFirst = modified.head
        modified = modified.drop(1)
        if (mFirst == sFirst) break

        result = result :+ Operation(Insert, Seq(mFirst))
      } }
      breakable { while (original.nonEmpty) {
        val oFirst = original.head
        original = original.drop(1)
        if (oFirst == sFirst) break
        result = result :+ Operation(Delete, Seq(oFirst.toString))
      } }
      result = result :+ Operation(Equals, Seq(sFirst.toString))
    }
    while (modified.nonEmpty) {
      val mFirst = modified.head
      modified = modified.drop(1)
      result = result :+ Operation(Insert, Seq(mFirst.toString))
    }
    while (original.nonEmpty) {
      val oFirst = original.head
      original = original.drop(1)
      result = result :+ Operation(Delete, Seq(oFirst.toString))
    }

    Diff(a, b, result)
  }

  /**
   * Eliminate operationally trivial equalities, then reorder and merge both edits and equalities.
   * @param diffs List[Operation]
   */
  def clean(diffs: List[Operation]): List[Operation] = {
    var buffer = diffs
    var changes = false
    var equalities = List.empty[Int] // Stack of indices where equalities are found
    var lastEquality = Seq.empty[String] // Always equal to equalities.last.text
    var currentIndex = 0
    var preInsert  = false // Is there an insert op before the last equality
    var preDelete  = false // Is there a delete op before the last equality
    var postInsert = false // Is there an insert op after the last equality
    var postDelete = false // Is there a delete op after the last equality

    while (currentIndex < buffer.length) {
      if (buffer(currentIndex).op == Equals) {
        if (buffer(currentIndex).text.length < MAX_EDIT_COST && (postInsert || postDelete)) {
          // Candidate found
          equalities   = equalities :+ currentIndex
          preInsert    = postInsert
          preDelete    = postDelete
          lastEquality = buffer(currentIndex).text
        }
        else {
          // Not a candidate
          equalities   = List.empty[Int]
          lastEquality = Seq.empty
        }
        postInsert = false
        postDelete = false
      }
      else { // An insertion or deletion
        buffer(currentIndex).op match {
          case Delete => postDelete = true
          case Insert => postInsert = true
        }

        /**
         * Five types to be split:
         * +A-BXY+C-D
         * +AX+C-D
         * +A-BX+C
         * +AX+C-D
         * +A-BX-C
         */
        val bools = Seq(preInsert, preDelete, postInsert, postDelete)
        if (lastEquality.nonEmpty && (bools.forall(b=>b) || ((lastEquality.length < MAX_EDIT_COST / 2) && bools.count(b => b) == 3))) {
          // Duplicate record
          // Change second copy to insert
          buffer     = replace(buffer, equalities.last, 1)(Operation(Delete, lastEquality), Operation(Insert, lastEquality))
          equalities = equalities.dropRight(1)
          lastEquality = Seq.empty
          if (preInsert && preDelete) {
            // No changes made which could affect previous entry, keep going
            postInsert = true
            postDelete = true
            equalities = List.empty[Int]
          }
          else {
            if (equalities.nonEmpty) {
              equalities   = equalities.dropRight(1) // Throw away previous equality
              currentIndex = equalities.lastOption.getOrElse(-1)
            }
            postInsert = false
            postDelete = false
          }
          changes = true
        }
      }
      currentIndex += 1
    }

    cleanMerge(buffer)
  }

  /**
   * Reorder and merge edits, equalities. Any edit section can move as long as it doesn't cross an equality.
   * @param diffs List of Operations
   */
  private def cleanMerge(diffs: List[Operation]): List[Operation] = {
    var buffer       = diffs :+ Operation(Equals, Seq.empty)
    var deletes      = 0
    var inserts      = 0
    var deleted      = Vector.empty[String]
    var inserted     = Vector.empty[String]
    var currentIndex = 0

    while (currentIndex < buffer.length) {
      buffer(currentIndex).op match {
        case Insert =>
          inserts      += 1
          inserted = inserted ++ buffer(currentIndex).text
          currentIndex += 1
        case Delete =>
          deletes      += 1
          deleted = deleted ++ buffer(currentIndex).text
          currentIndex += 1
        case Equals =>
          // Upon reaching an equality, check for prior redundancies
          if (deletes + inserts > 1) {
            if (deletes != 0 && inserts != 0) {
              // Factor out any common prefixes
              val prefixLength = commonPrefix(inserted, deleted)
              if (prefixLength != 0) {
                val idx = currentIndex - deletes - inserts
                if (idx > 0 && buffer(idx - 1).op == Equals) {
                  var op = buffer(idx - 1)
                  op     = op.copy(op.op, op.text ++ sliceLeft(inserted, prefixLength))
                  buffer = insert(buffer, idx)(op)
                }
                else {
                  buffer = Operation(Equals, sliceLeft(inserted, prefixLength)) +: buffer
                  currentIndex += 1
                }
                inserted = inserted.slice(prefixLength, inserted.length - 1)
                deleted  = deleted.slice(prefixLength, deleted.length - 1)
              }
              // Factor out any common suffixes
              val suffixLength = commonSuffix(inserted, deleted)
              if (suffixLength != 0) {
                var op = buffer(currentIndex)
                op     = op.copy(op.op, sliceRight(inserted, suffixLength) ++ op.text)
                buffer = insert(buffer, currentIndex)(op)
              }
            }

            // Delete the offending records and add the merged ones
            if (deletes == 0) {
              val start = currentIndex - deletes - inserts
              buffer    = replace(buffer, start, deletes + inserts)(Operation(Insert, inserted))
            }
            else if (inserts == 0) {
              val start = currentIndex - deletes - inserts
              buffer    = replace(buffer, start, deletes + inserts)(Operation(Delete, deleted))
            }
            else {
              val start = currentIndex - deletes - inserts
              buffer    = replace(buffer, start, deletes + inserts)(Operation(Delete, deleted), Operation(Insert, inserted))
            }

            currentIndex = (currentIndex - deletes - inserts) + (if (deletes == 0) 0 else 1) + (if (inserts == 0) 0 else 1) + 1
          }
          else if (currentIndex != 0 && buffer(currentIndex - 1).op == Equals) {
            // Merge this equality with the previous one
            val previous = buffer(currentIndex - 1)
            val current  = buffer(currentIndex)
            buffer = replace(buffer, currentIndex - 1, 2)(previous.copy(previous.op, previous.text ++ current.text))
            currentIndex -= 1
          }
          else {
            currentIndex += 1
          }

          inserts  = 0
          deletes  = 0
          inserted = Vector.empty
          deleted  = Vector.empty
      }
    }

    if (buffer.last.text.isEmpty) {
      buffer = buffer.dropRight(1) // Remove the dummy entry at the end
    }

    // Second pass: look for single edits surrounded on both sides by equalities
    // which can be shifted sideways to eliminate an equality
    // e.g. A<ins>BA</ins>C -> <ins>AB</ins>AC
    var changes = false
    currentIndex = 1

    // Intentionally ignore the first and last element (don't need checking)
    while (currentIndex < buffer.length - 1) {
      var previous = buffer(currentIndex - 1)
      var current  = buffer(currentIndex)
      var next     = buffer(currentIndex + 1)

      if (previous.op == Equals && next.op == Equals) {
        // This is a single edit surrounded by equalities
        if (current.text.endsWith(previous.text)) {
          current = current.copy(current.op, previous.text ++ sliceLeft(current.text, previous.text.length))
          next    = next.copy(next.op, previous.text ++ next.text)
          // Shift the edit over the previous equality
          buffer  = replace(buffer, currentIndex, 2)(current, next)
          changes = true
        }
        else {
          if (current.text.startsWith(next.text)) {
            // Shift the edit over the next equality
            previous = previous.copy(previous.op, previous.text ++ next.text)
            current  = current.copy(current.op, sliceRight(current.text, next.text.length) ++ next.text)
            buffer   = replace(buffer, currentIndex - 1, 2)(previous, current)
            changes  = true
          }
        }
      }

      currentIndex += 1
    }

    if (changes) cleanMerge(buffer) else buffer
  }

  /**
   * Generate the longest common subsequence between two strings
   * using the traceback approach to solving this problem
   * @param a First string
   * @param b Second string
   * @return Longest common subsequence
   */
  private def lcs(a: Seq[String], b: Seq[String]): Seq[String] = {
    // Empty pair of strings? No LCS...
    if (a.isEmpty || b.isEmpty) { Seq.empty }
    else {
      // Same string? LCS is the string itself..
      if (a == b) { a }
      else {
        // Construct the LCS matrix using the lengths of the subsequences,
        // this is done to reduce the memory needed to solve the problem
        val lengths = Array.ofDim[Int](a.size + 1,b.size + 1)
        for (i <- a.indices) {
          for (j <- b.indices) {
            if (a(i) == b(j)) {
              lengths(i + 1)(j + 1) = lengths(i)(j) + 1
            }
            else {
              lengths(i + 1)(j + 1) = Math.max(lengths(i + 1)(j),lengths(i)(j + 1))
            }
          }
        }

        // Starting from the last cell in the matrix, trace back towards the origin, accumulating commonalities
        val builder = new ArrayBuffer[String]()
        var (x, y) = (a.size, b.size)
        do {
          if      (lengths(x)(y) == lengths(x - 1)(y)) { x -= 1 }
          else if (lengths(x)(y) == lengths(x)(y - 1)) { y -= 1 }
          else {
            builder.append(a(x-1))
            x -= 1
            y -= 1
          }
        } while (x != 0 && y != 0)

        // Due to the traceback approach, we built the result in reverse
        builder.reverse
      }
    }
  }

  /**
   * Determine the common prefix of two strings
   * @param text1 First string.
   * @param text2 Second string.
   * @return The number of characters common to the start of each string.
   */
  private def commonPrefix(text1: Seq[String], text2: Seq[String]): Int = {
    // Performance analysis: http://neil.fraser.name/news/2007/10/09/
    val n = Math.min(text1.length, text2.length) - 1
    for (i <- 0 to n) {
      if (text1(i) != text2(i)) {
        return i
      }
    }
    n
  }

  /**
   * Determine the common suffix of two strings
   * @param text1 First string.
   * @param text2 Second string.
   * @return The number of characters common to the end of each string.
   */
  private def commonSuffix(text1: Seq[String], text2: Seq[String]): Int = {
    // Performance analysis: http://neil.fraser.name/news/2007/10/09/
    val (text1length, text2length) = (text1.length, text2.length)
    // Return early if strings are empty
    if (text1length == 0 || text2length == 0)
      return 0
    val n = Math.min(text1length, text2length) - 1
    for (i <- 1 to n) {
      if (text1(text1length - i) != text2(text2length - i)) {
        return i - 1
      }
    }
    n
  }
}
