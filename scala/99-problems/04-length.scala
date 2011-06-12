// find the length of a list

import scala.annotation.tailrec

// not tail recursive
def length[A](xs: List[A]): Int = xs match {
  case Nil => 0
  case h :: t => 1 + length(t)
}


// tail recursive accumulator version
@tailrec
def length2Acc[A](l: Int, xs: List[A]): Int = xs match {
  case Nil => l
  case _ :: t => length2Acc(l + 1, t)
}

def length2[A](xs: List[A]): Int = length2Acc(0, xs)

// builtin version
def length3[A](xs: List[A]): Int = xs.length

// map + fold version
def length4[A](xs: List[A]): Int = xs.map(x => 1).foldLeft(0)(_ + _)

