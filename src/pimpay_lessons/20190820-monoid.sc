trait Semigroup[A] {
  def op(a:A, b:A):A
}

trait Monoid[A] extends Semigroup[A] {
  def z:A
}

val intAdditionMonoid:Monoid[Int] = new Monoid[Int] {
  override def z: Int = 0
  override def op(a: Int, b: Int): Int = a + b
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

def spark[A](v:IndexedSeq[A], m:Monoid[A]):A = ???