// create a list containing all integers within a given range

import scala.annotation.tailrec

// recursive version
def range(start: Int, end: Int): List[Int] = {
  if (start == end) {
    Nil
  } else {
    start :: range(start + 1, end)
  }
}

// iterative version
def range2(start: Int, end: Int): List[Int] = {
  var l: List[Int] = Nil
  var i = end - 1
  do {
    l ::= i
    i -= 1
  } while (i >= start)
  l
}

@tailrec
def range3Acc(start: Int, end: Int, l: List[Int]): List[Int] = {
  if (start == end) {
    start :: l
  } else {
    range3Acc(start, end - 1, end :: l)
  }
}
def range3(start: Int, end: Int): List[Int] = range3Acc(start, end - 1, Nil)

// vim: set ts=2 sw=2 sts=2 et:
