// duplicate the elements of a list

// naive recursive
def duplicate[A](l: List[A]): List[A] = l match {
  case Nil => Nil
  case h :: t => h :: h :: duplicate(t)
}

// using flatmap
def duplicate2[A](l: List[A]): List[A] = l.flatMap(x => List(x, x))

// vim: set ts=2 sw=2 sts=2 et:
