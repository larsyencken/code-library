// draw n different random numbers in the range 1..M

import scala.util.Random

// naive O(m) version
def lotto(n: Int, m: Int): List[Int] = {
  Random.shuffle(1 to m) take n
}

// vim: set ts=2 sw=2 sts=2 et:
