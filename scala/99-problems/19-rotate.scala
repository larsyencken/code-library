// rotate a list n places to the left

// using slice
def rotate[A](n: Int, l: List[A]): List[A] = {
  val s = l.size
  if (n >= 0) {
    l.slice(n, s) ::: l.slice(0, n)
  } else {
    l.slice(s + n, s) ::: l.slice(0, s + n) 
  }
}

// using drop and take
def rotate2[A](n: Int, l: List[A]): List[A] = {
  if (n >= 0) {
    l.drop(n) ::: l.take(n)
  } else {
    val s = l.size
    l.drop(s + n) ::: l.take(s + n)
  }
}

// vim: set ts=2 sw=2 sts=2 et:
