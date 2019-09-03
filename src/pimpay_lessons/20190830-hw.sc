trait Semigroup[A] {
  def op(a:A, b:A):A
}

trait Monoid[A] extends Semigroup[A] {
  def z:A
}

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


def foldMap[A, B](seq:Seq[A])(f: A => B)(implicit m:Monoid[B]):B = seq.foldLeft(m.z)((acc, el) => m.op(acc, f(el))) // imagine it as balanced and parallel

// ADT
trait Chunk[+A]
case class SortedChunk[A](bounds:Option[(A,A)] = Option.empty) extends Chunk[A]
case object UnsortedChunk extends Chunk[Nothing]

// for map phase
object Chunk {
  def apply[A : Ordering](seq:Seq[A]):Chunk[A] = if (seq.length <= 1) SortedChunk(seq.headOption.map(h => h -> h)) else {
    val ord = implicitly[Ordering[A]]
    val unorderedPair = seq.sliding(2).find(s => ord.gt(s.head, s.last))
    unorderedPair.map(_ => UnsortedChunk).getOrElse(SortedChunk(Option(seq.head -> seq.last)))
  }
}

// monoid
implicit def isSortedChunkMonoid[A : Ordering]:Monoid[Chunk[A]] = new Monoid[Chunk[A]] {
  override def z: Chunk[A] = SortedChunk[A]()
  override def op(a: Chunk[A], b: Chunk[A]): Chunk[A] = (a,b) match {
    case (SortedChunk(aBounds), SortedChunk(bBounds)) => compareBounds(aBounds, bBounds)
    case _ => UnsortedChunk
  }

  def compareBounds(aBounds:Option[(A,A)], bBounds:Option[(A,A)]):Chunk[A] = (aBounds, bBounds) match {
    case (Some((aMin, aMax)), Some((bMin, bMax))) => if (implicitly[Ordering[A]].lteq(aMax, bMin)) SortedChunk(Option(aMin -> bMax)) else UnsortedChunk
    case (Some(_), _) => SortedChunk(aBounds)
    case (_, Some(_)) => SortedChunk(bBounds)
    case _ => SortedChunk()
  }
}

def isSorted[A : Ordering](chunks:Seq[Seq[A]]):Boolean = foldMap(chunks)(Chunk[A])(isSortedChunkMonoid) != UnsortedChunk

isSorted(Seq(1,2,3,4,5,6,7,8,9,10).grouped(3).toSeq)
isSorted(Seq(1).grouped(3).toSeq)
isSorted(Seq[Int]().grouped(3).toSeq)
isSorted(Seq(7,6,5).grouped(3).toSeq)
isSorted(Seq(2,3,5,6,1).grouped(3).toSeq)
isSorted(Seq(1,1,1,1).grouped(3).toSeq)