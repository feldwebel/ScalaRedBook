import scala.concurrent.{Future, Await}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

trait Semigroup[A] {
  def op(a:A, b:A):A
}

trait Monoid[A] extends Semigroup[A] {
  def z:A
}

implicit def intAdditionMonoid[N: Numeric]:Monoid[N] = new Monoid[N] {
  val n = implicitly[Numeric[N]]
  override def z: N = n.zero
  override def op(a: N, b: N): N = n.plus(a, b)
}

val intMultiplicationMonoid:Monoid[Int] = new Monoid[Int] {
  override def z: Int = 1
  override def op(a: Int, b: Int): Int = a * b
}

val stringConcatMonoid:Monoid[String] = new Monoid[String] {
  override def z: String = ""
  override def op(a: String, b: String): String = a + b
}

/*
val booleanAndMonoid:Monoid[Boolean] = ???
val booleanOrMonoid:Monoid[Boolean] = ???
*/

def foldRight[A](list:List[A])(z:A)(op: (A,A) => A):A = list.foldRight(z)(op)

def foldWithMonoid[A](list:List[A])(m:Monoid[A]):A = list.foldRight(m.z)(m.op)

val sum = foldWithMonoid(List(1,2,3))(intAdditionMonoid)
val prod = foldWithMonoid(List(1,2,5))(intMultiplicationMonoid)

implicit class ListOps[A](list:List[A]) {
  def foldUsing(m:Monoid[A]):A = foldWithMonoid(list)(m)
}


List(1,2,3) foldUsing intAdditionMonoid


// a + b + c + d
//a + (b + (c + d))
// (a + b) + (c + d)


def spark[A](v:IndexedSeq[A])(m:Monoid[A]):Future[A] = {
  v.length match {
    case 0 => Future.successful(m.z)
    case 1 => Future.successful(v(0))
    case _ => {
      val (left, right) = v.splitAt(v.length >> 1)
      val lf = spark(left)( m)
      val rf = spark(right)(m)
      (lf zip rf).map((m.op _).tupled)
    }
  }
}

val p1 = Await.result(spark(IndexedSeq(1, 2, 3, 4))( intAdditionMonoid), Duration.Inf) // 10
val p2 = Await.result(spark(IndexedSeq(1, 2, 3, 4, 5))( intMultiplicationMonoid), Duration.Inf) // 120

def foldMap[A, B](s: Seq[A])(m: Monoid[B])(f: A => B): B =
  s.foldLeft(m.z)((a, el) => m.op(a, f(el)))

def length[A](s: Seq[A]): Int = foldMap(s)(intAdditionMonoid)(_ => 1)
def sum(s: Seq[Int]): Int = foldMap(s)(intAdditionMonoid)(identity)

def avt(s: Seq[Int]): Double = sum(s) / length(s)

def avt1(s: Seq[Int]): Double = {
 val (su, le) = foldMap(s)(product(intAdditionMonoid, intAdditionMonoid))(_ -> 1)
  su / le
}

length(List(1, 2, 5))

sum(List(1, 2, 5))

avt1(List(1, 2, 5))

def product[A, B](ma: Monoid[A], mb: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
  override def z: (A, B) = ma.z -> mb.z
  override def op(a: (A, B), b: (A, B)): (A, B) = ma.op(a._1, b._1) -> mb.op(a._2, b._2)
}

implicit def endofunctionMonoid[A]:Monoid[A => A] = new Monoid[A=>A]{
  override def z: (A) => A = identity
  override def op(a: (A) => A, b: (A) => A): (A) => A = a andThen b
}

def srt(i:Int):Int = i * i
def twice(i:Int):Int = i * 2

implicit class MonoidSyntax[A: Monoid](a: A) {
  def |@| (b: A):A = implicitly[Monoid[A]].op(a, b)
}

(srt _ |@| twice)(3)
//2 |@| 3


implicit def mapMergeMonoid[K, V: Monoid]:Monoid[Map[K,V]] = new Monoid[Map[K, V]] {
  override def z: Map[K, V] = Map[K, V]()
  override def op(a: Map[K, V], b: Map[K, V]): Map[K, V] = {
    val v = implicitly[Monoid[V]]
    var acc = Map[K, V]()
    (a.keySet union b.keySet).map((i: K) => {acc + (i -> v.op(a.getOrElse(i, v.z), b.getOrElse(i, v.z)))})
    acc
  }

}


val lim1 = Map("sdek" -> 100.0, "beta" -> 20.00)
val lim2 = Map("beta" -> 30.0, "dpd" -> 4)

lim1 |@| lim2 // Map("sdek" -> 100, "beta" -> 50, "dpd" -> 4)