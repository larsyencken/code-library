// remove the kth element from a list

def removeAt[A](k: Int, l: List[A]): Tuple2[List[A], A] = {
  val x = l.slice(0, k)
  val y = l.slice(k, l.size)
  (x ::: y.tail, y.head)
}

// vim: set ts=2 sw=2 sts=2 et:
