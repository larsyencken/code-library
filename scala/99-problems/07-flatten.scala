// flatten a nested list structure

import scala.annotation.tailrec

// functional, but not tail recursive 
def flatten(xs: List[Any]): List[Any] = xs match {
  case Nil => Nil
  case h :: t => h match {
    case hs: List[_] => flatten(hs ::: t)
    case _ => h :: flatten(t)
  }
}

// tail recursive accumulator version
@tailrec
def flatten2Acc(xs: List[Any], ys: List[Any]): List[Any] = xs match {
  case Nil => ys.reverse
  case h :: t => h match {
    case hs: List[_] => flatten2Acc(hs ::: t, ys)
    case _ => flatten2Acc(t, h :: ys)
  }
}
def flatten2(xs: List[Any]): List[Any] = flatten2Acc(xs, Nil)

// builtin version using flatMap
def flatten3(xs: List[Any]): List[Any] = xs flatMap {
  case ms: List[_] => flatten3(ms)
  case e => List(e)
}

// vim: set ts=2 sw=2 sts=2 et:
