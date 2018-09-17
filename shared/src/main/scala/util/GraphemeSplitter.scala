package util

import java.util

import scala.collection.mutable.ArrayBuffer

object GraphemeSplitter {

  import UnicodeGraphemeBreakData._
  private def graphemeBreakType(i: Int) = {
    val a = util.Arrays.binarySearch(UnicodeGraphemeBreakData.starts, i)
    if (a >= 0) {
      UnicodeGraphemeBreakData.cats(a)
    } else {
      val test = -a -1
      if (test == 0) {
        -1
      } else {
        if (i <= UnicodeGraphemeBreakData.ends(test - 1)) {
          UnicodeGraphemeBreakData.cats(test - 1)
        } else {
          -1
        }
      }
    }
  }

//  def prevBreak(a: String, from: Int): Int = {
//
//  }

  def isLvt(tn: Int): Boolean = {
    return tn == L || tn == LV || tn == LVT || tn == V || tn == T
  }


  def nextBreak(a: String, from: Int = 0): Int = {
    val code = a.codePointAt(from)
    var ff = from
    var tcode = graphemeBreakType(code)
    var hasRiBefore = false
    while (true) {
      val middleIndex = a.offsetByCodePoints(ff, 1)
      if (middleIndex == a.length) {
        return middleIndex
      } else {
        val next = a.codePointAt(middleIndex)
        val tn = graphemeBreakType(next)
        if (tcode == CR && tn == LF) {
          tcode = tn
          ff = middleIndex
        } else if (tcode == CR || tcode == Control || tcode == LF) {
          return middleIndex
        } else if (tn == CR || tn == Control || tn == LF) {
          return middleIndex
        } else if ((tcode == L) && (tn == L || tn == V || tn == LV || tn == LVT)) {
          tcode = tn
          ff = middleIndex
        } else if ((tcode == LV || tcode == V) && (tn == V || tn == T)) {
          tcode = tn
          ff = middleIndex
        } else if ((tcode == LVT || tcode == T) && (tn == T)) {
          tcode = tn
          ff = middleIndex
        } else if (Extend == tn || ZWJ == tn) {
          tcode = tn
          ff = middleIndex
        } else if (tn == SpacingMark) {
          tcode = tn
          ff = middleIndex
        } else if (Prepend == tcode) {
          tcode = tn
          ff = middleIndex
        } else if (ZWJ == tcode && tn == pExtended_Pictographic) {
          var kk = ff
          while (kk > 0) {
             val offset = a.offsetByCodePoints(kk, -1)
            val ty = graphemeBreakType(a.codePointAt(offset))
            if (ty == Extend) {
              kk = offset
            } else if (ty == pExtended_Pictographic) {
              kk = -1
            } else {
              return middleIndex
            }
          }
          if (kk == 0) {
            return middleIndex
          } else {
            tcode = tn
            ff = middleIndex
          }
        } else if (tcode == Regional_Indicator) {
          if (hasRiBefore) {
            return middleIndex
          } else if (tn == Regional_Indicator) {
            ff = middleIndex
            hasRiBefore = true
          } else {
            return middleIndex
          }
        } else {
          return middleIndex
        }
      }
    }
    throw new IllegalArgumentException("Not possible here!")
  }
}
