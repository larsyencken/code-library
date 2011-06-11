// find the kth element of a list

import scala.annotation.tailrec

@tailrec
def kthElem[A](k: Int, xs: List[A]): A = {
  assert(k >= 0)
  k match {
    case 0 => xs.head
    case _ => kthElem(k - 1, xs.tail)
  }
}

def kthElem2[A](k: Int, xs: List[A]): A = xs(k)
