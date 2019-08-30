def mapReduce[A,B](seq:Seq[A])(f: A => B)(implicit m: Monoid[B]): B = seq.foldLeft(m.z)((acc, el) => m.op(acc, f(el)))


def findMax[A: Ordering](chunks:Seq[Seq[A]]): Option[A] = {
  val order = implicitly[Ordering[A]]
  val maxMonoid:Monoid[Option[A]] = new Monoid[Option[A]] {
    override def z: Option[A] = None
    override def op(a: Option[A], b: Option[A]): Option[A] = (a, b) match {
      case (Some(a), Some(b)) => Some(order.max(a, b))
      case (None, Some(_)) => b
      case (Some(_), None) => a
      case _ => z
    }
    //a.fold(b)(av => b.fold(a)(bv =
  }
  mapReduce(chunks)(c => if (c.isEmpty) Option.empty[A] else Option(c.max))(maxMonoid)
}

findMax(Seq(Seq(1, 392), Seq(22, 0)))
findMax(Seq(Seq(), Seq()))

case class Sorted[A: Ordering](isSorted: Boolean, min: Option[A], max: Option[A])

def isSorted[A: Ordering](chunks:Seq[Seq[A]]):Boolean = {
  val order = implicitly[Ordering[A]]
  val isSortedMonoid: Monoid[Sorted[A]] = new Monoid[Sorted[A]] {

  }
}