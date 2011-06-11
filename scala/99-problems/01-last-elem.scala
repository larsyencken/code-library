// find the last element of a list

import scala.annotation.tailrec

@tailrec
def last[A](ls: List[A]): A = ls match {
  case h :: Nil => h
  case _ :: rest => last(rest)
  case _ => error("list must be non-empty")
}

def last2[A](ls: List[A]): A = ls.last
