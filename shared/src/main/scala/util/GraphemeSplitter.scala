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

  def isLvt(tn: Int): Boolean = {
    return tn == L || tn == LV || tn == LVT || tn == V || tn == T
  }

  def prevBreak(a: String, from: Int): Int = {
    if (from == 0) {
      0
    } else {
      var ff = a.offsetByCodePoints(from, -1)
      var tn = graphemeBreakType(a.codePointAt(ff))
      var nn = from
      while (true) {
        if (ff == 0) {
          return 0
        } else {
          val ss = a.offsetByCodePoints(ff, -1)
          var tcode = graphemeBreakType(a.codePointAt(ss))
          if (tcode == CR && tn == LF) {
            tn = tcode
            nn = ff
            ff = ss
          } else if (tcode == CR || tcode == Control || tcode == LF) {
            return ff
          } else if (tn == CR || tn == Control || tn == LF) {
            return ff
          } else if (isLvt(tn) && isLvt(tcode)) {
            var ik = ss
            var lvtStart = -1
            while (lvtStart == -1 && ik > 0) {
              val nik = a.offsetByCodePoints(ik, -1)
              if (isLvt(graphemeBreakType(a.codePointAt(nik)))) {
                ik = nik
              } else {
                lvtStart = ik
              }
            }
            if (ik == 0) {
              lvtStart = 0
            }
            var prev = lvtStart
            var next = lvtStart
            while (next < nn) {
              prev = next
              next = nextBreak(a, next)
            }
            if (prev == lvtStart) {
              tn = L // doesn't mater now, just pick up some prepend
              ff = prev
              nn = next // donesn't mater now
            } else {
              return prev
            }
          } else if (Extend == tn || ZWJ == tn) {
            tn = tcode
            nn = ff
            ff = ss
          } else if (tn == SpacingMark) {
            tn = tcode
            nn = ff
            ff = ss
          } else if (Prepend == tcode) {
            tn = tcode
            nn = ff
            ff = ss
          } else if (ZWJ == tcode && tn == pExtended_Pictographic) {
            var kk = ss
            while (kk > 0) {
              val offset = a.offsetByCodePoints(kk, -1)
              val ty = graphemeBreakType(a.codePointAt(offset))
              if (ty == Extend) {
                kk = offset
              } else if (ty == pExtended_Pictographic) {
                tn = pExtended_Pictographic
                ff = offset
                nn = kk
                kk = -1
              } else {
                return ff
              }
            }
            if (kk == 0) {
              return 0
            }
          } else if (tcode == Regional_Indicator && tn == Regional_Indicator) {
            var kk = ss
            var beforeHasRi = 0
            while (kk > 0) {
              val offset = a.offsetByCodePoints(kk, -1)
              val ty = graphemeBreakType(a.codePointAt(offset))
              if (ty == Regional_Indicator) {
                beforeHasRi += 1
                kk = offset
              } else {
                kk = -1
              }
            }
            if (beforeHasRi % 2 == 1) {
              return ff
            } else {
              tn = Regional_Indicator
              ff = ss
              nn = ff
            }
          } else {
            return ff
          }
        }
      }
    }
    throw new IllegalArgumentException("Not possible here!")
  }

  def nextBreak(a: String, from: Int = 0): Int = {
    if (from == a.length) return a.length
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
