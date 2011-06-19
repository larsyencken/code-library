// generate a random permutation of the elements of a list

import scala.util.Random

def removeAt[A](k: Int, l: List[A]): Tuple2[List[A], A] = {
  val x = l.slice(0, k)
  val y = l.slice(k, l.size)
  (x ::: y.tail, y.head)
}

def randomPermute[A](l: List[A])(implicit m: ClassManifest[A]): List[A] = {
  val a = l.toArray
  0.until(a.size).foreach(i => {
    val j = Random.nextInt(a.size)
    if (i != j) {
      val tmp = a(i)
      a(i) = a(j)
      a(j) = tmp
    }
  })
  a.toList
}

// vim: set ts=2 sw=2 sts=2 et:
