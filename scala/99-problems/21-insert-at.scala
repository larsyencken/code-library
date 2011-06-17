// insert an element at a given position into a list

def insertAt[A](x: A, k: Int, l: List[A]): List[A] = {
  l.take(k) ::: x :: l.drop(k)
}

def insertAt2[A](x: A, k: Int, l: List[A]): List[A] = {
  val (pre, post) = l.splitAt(k)
  pre ::: x :: post
}

// vim: set ts=2 sw=2 sts=2 et:
