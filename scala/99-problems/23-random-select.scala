// extract a given number of randomly selected elements

import scala.util.Random

def randomSelect[A](k: Int, l: List[A]): List[A] = {
  Random.shuffle(l).take(k)
}

def removeAt[A](k: Int, l: List[A]): Tuple2[List[A], A] = {
  val x = l.slice(0, k)
  val y = l.slice(k, l.size)
  (x ::: y.tail, y.head)
}
def randomSelect2[A](k: Int, l: List[A]): List[A] = {
  var r: List[A] = Nil
  var y = l
  var xs: List[A] = l
  var i = k
  while (i > 0) {
    val popped = removeAt(Random.nextInt(xs.length), xs)
    r ::= popped._2
    xs = popped._1
    i -= 1
  }
  r
}

// vim: set ts=2 sw=2 sts=2 et:
