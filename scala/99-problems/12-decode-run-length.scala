// decode a run-length encoded list

def decode[A](l: List[Tuple2[Int, A]]): List[A] = l.flatMap(t => {
    val (n, x) = t
    0.until(n).map(_ => x)
  })
}

// vim: set ts=2 sw=2 sts=2 et:
