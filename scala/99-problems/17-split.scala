// split a list into two parts by length

def split[A](n: Int, l: List[A]): Tuple2[List[A], List[A]] = {
  val (left, right) = l.zipWithIndex.span(_._2 < n)
  (left.map(_._1), right.map(_._1))
}

// vim: set ts=2 sw=2 sts=2 et:
