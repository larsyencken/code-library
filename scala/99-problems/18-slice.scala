// extract a slice from a list

def slice[A](start: Int, end: Int, l: List[A]): List[A] = {
  l.zipWithIndex.dropWhile(_._2 < start).takeWhile(_._2 < end).map(_._1)
}

// vim: set ts=2 sw=2 sts=2 et:
