// reverse a list

import scala.annotation.tailrec

// accumulator recursion
@tailrec
def reverseAcc[A](xs: List[A], ys: List[A]): List[A] = xs match {
  case Nil => ys
  case h :: t => reverseAcc(t, h :: ys)
}
def reverse[A](xs: List[A]): List[A] = reverseAcc(xs, Nil)

// builtin version
def reverse2[A](xs: List[A]): List[A] = xs.reverse

// foldl version
def reverse3[A](xs: List[A]): List[A] = xs.foldLeft(Nil:List[A])((ys, y) => y :: ys)

// vim: set ts=2 sw=2 sts=2 et:
