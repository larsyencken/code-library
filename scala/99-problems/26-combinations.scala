// generate the combinations of k distinct objects from n elements

def removeAt[A](k: Int, l: List[A]): Tuple2[List[A], A] = {
  val x = l.slice(0, k)
  val y = l.slice(k, l.size)
  (x ::: y.tail, y.head)
}

def combinations[A](k: Int, l: List[A]): List[List[A]] = {
  assert(k >= 1)
  k match {
    case 1 => {
      for (e <- l) yield { List(e) }
    }
    case _ => {
      val s = l.size
      (for (
        i <- 0.until(s - k) ;
        val end = l.drop(i) ;
        rest <- combinations(k - 1, end.tail)
      ) yield {
        end.head :: rest
      }).toList
    }
  }
}

// vim: set ts=2 sw=2 sts=2 et:
