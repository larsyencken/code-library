// duplicate a list's elements n times

def duplicateN[A](n: Int, l: List[A]) = l flatMap {
  x => 0.until(n).map(i => x)
}

def duplicateN2[A](n: Int, l: List[A]) = l flatMap { List.make(n, _) }

// vim: set ts=2 sw=2 sts=2 et:
