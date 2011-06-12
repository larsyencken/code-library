// pack consecutive duplicates into sublists

import scala.annotation.tailrec

def pack[A](l: List[A]): List[List[A]] = l match {
  case Nil => Nil
  case h :: t => (h :: t.takeWhile(_ == h)) :: pack(t.dropWhile(_ == h))
}

@tailrec
def pack2Acc[A](l: List[A], acc: List[List[A]]): List[List[A]] = l match {
  case Nil => acc.reverse
  case h :: t => pack2Acc(
    t.dropWhile(_ == h), 
    (h :: t.takeWhile(_ == h)) :: acc
  )
}
def pack2[A](l: List[A]): List[List[A]] = pack2Acc(l, Nil)

def pack3[A](l: List[A]): List[List[A]] = {
  if (l.isEmpty) {
    Nil
  } else {
    val (packed, next) = l.span(_ == l.head)
    packed :: pack3(next)
  }
}

@tailrec
def pack4Acc[A](l: List[A], acc: List[List[A]]): List[List[A]] = l match {
  case Nil => acc.reverse
  case h :: t => {
    val (packed, next) = l.span(_ == h)
    pack4Acc(next, packed :: acc)
  }
}
def pack4[A](l: List[A]): List[List[A]] = pack4Acc(l, Nil)

// vim: set ts=2 sw=2 sts=2 et:
