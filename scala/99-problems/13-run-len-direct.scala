// implement run-length encoding directly

import scala.annotation.tailrec

// naive recursive
def encode[A](l: List[A]): List[Tuple2[Int, A]] = l match {
  case Nil => Nil
  case h :: _ => {
    val (run, rest) = l.span(_ == h)
    (run.length, h) :: encode(rest)
  }
}

@tailrec
def encode2Acc[A](l: List[A], acc: List[Tuple2[Int, A]]): List[Tuple2[Int, A]] = l match {
  case Nil => acc.reverse
  case h :: _ => {
    val (run, rest) = l.span(_ == h)
    encode2Acc(rest, (run.length, h) :: acc)
  }
}
def encode2[A](l: List[A]): List[Tuple2[Int, A]] = encode2Acc(l, Nil)

// vim: set ts=2 sw=2 sts=2 et:
