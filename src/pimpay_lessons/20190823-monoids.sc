import scala.concurrent.{ExecutionContext, Future}
import scala.reflect.runtime.universe._

trait Semigroup[A] {
  def op(a:A, b:A):A
}

trait Monoid[A] extends Semigroup[A] {
  def z:A
}

implicit def numericAdditionMonoid[N : Numeric]:Monoid[N] = new Monoid[N] {
  val n:Numeric[N] = implicitly[Numeric[N]]
  override def z: N = n.zero
  override def op(a: N, b: N): N = n.plus(a,b)
}


val intMultiplicationMonoid:Monoid[Int] = new Monoid[Int] {
  override def z: Int = 1
  override def op(a: Int, b: Int): Int = a * b
}

implicit def stringConcatMonoid:Monoid[String] = new Monoid[String] {
  override def z: String = ""
  override def op(a: String, b: String): String = a + b
}

/*
val booleanAndMonoid:Monoid[Boolean] = ???
val booleanOrMonoid:Monoid[Boolean] = ???
*/

def foldRight[A](list:List[A])(z:A)(op: (A,A) => A):A = list.foldRight(z)(op)

def foldWithMonoid[A](list:List[A])(implicit m:Monoid[A]):A = list.foldRight(m.z)(m.op)

val sum = foldWithMonoid(List(1,2,3))
val prod = foldWithMonoid(List(1,2,5))(intMultiplicationMonoid)

implicit class ListOps[A](list:List[A]) {
  def foldUsing(m:Monoid[A]):A = foldWithMonoid(list)(m)
}


List(1,2,3) foldUsing numericAdditionMonoid[Int]

// a + b + c + d
// a + (b + (c + d))
// (a + b) + (c + d)

def spark[A](v:IndexedSeq[A])(m:Monoid[A])(implicit ec:ExecutionContext):Future[A] = {
  if (v.length <= 1) Future.successful(v.headOption.getOrElse(m.z)) else {
    val (l,r) = v.splitAt(v.length/2)
    val f = spark(l)(m) zip spark(r)(m)
    f.map((m.op _).tupled)
  }
}

spark(Vector(1,2,3,5,6))(numericAdditionMonoid[Int])(scala.concurrent.ExecutionContext.Implicits.global)

def foldMap[A,B](s:Seq[A])(m:Monoid[B])(f: A => B):B = s.foldLeft(m.z)((acc, el) => m.op(acc, f(el)))

def length[A](s:Seq[A]):Int = foldMap(s)(numericAdditionMonoid[Int])(_ => 1)
def sum(s:Seq[Int]):Int = foldMap(s)(numericAdditionMonoid[Int])(identity)

def avg(s:Seq[Double]):Double = {
  val (sum,length) = foldMap(s)(product(numericAdditionMonoid[Double],numericAdditionMonoid[Double]))(_ -> 1)
  sum.toDouble / length
}

length(List(1,2,4))

implicit def product[A,B](ma:Monoid[A], mb:Monoid[B]):Monoid[(A,B)] = new Monoid[(A,B)] {
  override def z: (A, B) = ma.z -> mb.z
  override def op(a: (A, B), b: (A, B)): (A, B) = ma.op(a._1, b._1) -> mb.op(a._2, b._2)
}

implicit def endofunctionMonoid[A]:Monoid[A=>A] = new Monoid[A=>A] {
  override def z: (A) => A = identity
  override def op(a: (A) => A, b: (A) => A): (A) => A = a andThen b
}

def sqr(i:Int):Int = i * i
def double(i:Int):Int = i * 2

implicit class MonoidSyntax[A : Monoid](a:A) {
  def |@|(b:A):A = implicitly[Monoid[A]].op(a,b)
}

(sqr _ |@| double) (3)

2 |@| 3 // new MonoidSyntax[Int](2)(intAdditionMonoid).|@|(3)

implicit def mapMergeMonoid[K, V : Monoid]: Monoid[Map[K,V]] = new Monoid[Map[K, V]] {
  override def z = Map.empty[K, V]
  override def op(a: Map[K, V], b: Map[K, V]): Map[K, V] = {
    val v = implicitly[Monoid[V]]
    (a.toSeq ++ b.toSeq).groupBy(_._1).mapValues(_.map(_._2).fold(v.z)(v.op))
  }
}

val limitCalculator1 = Map("sdek" -> 100, "beta" -> 20)
val limitCalculator2 = Map("beta" -> 30, "dpd" -> 4)

limitCalculator1 |@| limitCalculator2 // Map("sdek" -> 100, "beta" -> 50, "dpd" -> 4)

2.0 |@| 5.0
"hello" |@| "world"

implicit def OptionMonoid[A: Monoid]: Monoid[Option[A]] = new Monoid[Option[A]] {
  val m: Monoid[A] = implicitly[Monoid[A]]
  override def z = Option.empty[A]
  override def op(a: Option[A], b: Option[A]) = (a, b) match {
    case (Some(aa), Some(bb)) => Option(m.op(aa, bb))
    case (Some(_), _) => a
    case (_, Some(_)) => b
    case _ => z
  }
}

val pings = List(1, 2, 3, 4).map(_ * 2.4).map(Option(_).filter(_ > 4))

foldWithMonoid(pings)

implicit def seqMonoid[A]: Monoid[Seq[A]] = new Monoid[Seq[A]] {
  override def z = Seq.empty[A]

  override def op(a: Seq[A], b: Seq[A]) = a ++ b
}


type Server = String
type Ping = Option[Double]
type ListOfPings = Seq[Ping]

def watch1:Map[Server,ListOfPings] = Map(
  "admin"   ->  { Seq(1,2,3,4) map (_ * 2.4) map (Option(_).filter(_ > 4)) },
  "api"     ->  { Seq(3,58,12,4) map (_ * 2.4) map (Option(_).filter(_ > 21)) },
  "cabinet" ->  { Seq(58,7,4) map (_ * 2.4) map (Option(_).filter(_ > 15)) }
)

def watch2:Map[Server,ListOfPings] = Map(
  "admin"   ->  { Seq(182,3,44) map (_ * 2.4) map (Option(_).filter(_ > 4)) },
  "cabinet" ->  { Seq(58,7,4) map (_ * 2.4) map (Option(_).filter(_ > 15)) }
)

val r = (watch1 |@| watch2) mapValues (l => l.collect{case Some(d) => d } ) mapValues avg

/*def foldSeqWithMonoid[A](s:Seq[A])(implicit m:Monoid[A]):A = s.foldRight(m.z)(m.op)

implicit def isSortedMonoid:Monoid[Boolean] = new Monoid[Boolean] {
  override def z: Boolean = false
  override def op(a: Boolean, b: Boolean): Boolean =
}
def isSorted[A](chunks:IndexedSeq[Seq[A]]):Boolean = foldSeqWithMonoid(chunks)(isSortedMonoid)


isSorted(IndexedSeq(Seq(1,2,3),Seq(4,5,6),Seq(7))) == true
isSorted(IndexedSeq(Seq(1,2,3),Seq(2,4,5,6),Seq(7))) == false*/


