// eliminate consecutive duplicates from a list
//
// scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
// res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)

import scala.annotation.tailrec

// non-tail recursive
def compress[A](l: List[A]): List[A] = l match {
  case x :: y :: ys => if (x == y) {
    compress(y :: ys)
  } else {
    x :: compress(y :: ys)
  }
  case xs => xs
}

// accumulator tail recursion
@tailrec
def compress2Acc[A](l: List[A], r: List[A]): List[A] = l match {
  case x :: y :: ys => if (x == y) {
    compress2Acc(y :: ys, r)
  } else {
    compress2Acc(y :: ys, x :: r)
  }
  case xs => (xs ::: r).reverse
}
def compress2[A](l: List[A]): List[A] = compress2Acc(l, Nil)

// functional
def compress3[A](l: List[A]): List[A] = l.foldRight(Nil:List[A]) { (h, r) =>
  if (r.isEmpty || r.head != h) {
    h :: r
  } else {
    r
  }
}

// naive recursive, using dropWhile
def compress4[A](l: List[A]): List[A] = l match {
  case Nil => Nil
  case h :: t => h :: compress4(t.dropWhile(_ == h))
}

// vim: set ts=2 sw=2 sts=2 et:
