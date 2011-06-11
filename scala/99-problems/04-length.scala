// find the length of a list

import scala.annotation.tailrec

// not tail recursive
def length[A](xs: List[A]): Int = xs match {
  case Nil => 0
  case h :: t => 1 + length(t)
}
