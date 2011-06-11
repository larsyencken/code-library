// find the penultimate element of a list

import scala.annotation.tailrec

@tailrec
def penultimate[A](ls: List[A]): A = ls match {
  case a :: _ :: Nil => a
  case _ :: bs => penultimate(bs)
  case _ => error("list must contain at least two elements")
}

def penultimate2[A](ls: List[A]): A = ls.init.last

def nthLast[A](n: Int, ls: List[A]): A = ls.takeRight(n).head

def penultimate3[A](ls: List[A]) = nthLast(1, ls)

