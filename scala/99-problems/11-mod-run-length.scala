// modified run-length encoding

import scala.annotation.tailrec

@tailrec
def packAcc[A](l: List[A], acc: List[List[A]]): List[List[A]] = l match {
  case Nil => acc.reverse
  case h :: t => {
    val (packed, next) = l.span(_ == h)
    packAcc(next, packed :: acc)
  }
}
def pack[A](l: List[A]): List[List[A]] = packAcc(l, Nil)

def encodeModified[A](l: List[A]): List[Any] = {
  pack(l).map(x => x match {
    case h :: Nil => h
    case __ => (x.length, x.head)
  })
}

// vim: set ts=2 sw=2 sts=2 et:
