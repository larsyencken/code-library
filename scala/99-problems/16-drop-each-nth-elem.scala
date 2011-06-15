// drop every nth element from a list

def drop[A](n: Int, l: List[A]): List[A] = for {
  (e, i) <- l.zipWithIndex
  if ((i + 1) % n != 0)
} yield { e }

// vim: set ts=2 sw=2 sts=2 et:
