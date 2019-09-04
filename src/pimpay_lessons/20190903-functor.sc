import scala.languageFeature.higherKinds
import scala.languageFeature.reflectiveCalls


trait Functor[F[_]] { // HKT
  def map[A,B](fa:F[A])(f:A=>B):F[B]

  def lift[A,B](f:A=>B):F[A] => F[B] = map(_)(f)

  def distribute[A,B](fab:F[(A,B)]):(F[A], F[B]) = map(fab)(_._1) -> map(fab)(_._2)

  def as[A,B](fa:F[A], b:B):F[B] = map(fa)(_=>b)

  def fproduct[A,B](fa:F[A])(f:A=>B):F[(A,B)] = map(fa)(a => a -> f(a))
}

implicit val listFunctor:Functor[List] = new Functor[List] {
  override def map[A, B](fa: List[A])(f: (A) => B): List[B] = fa map f
}

implicit val optionFunctor:Functor[Option] = new Functor[Option] {
  override def map[A, B](fa: Option[A])(f: (A) => B): Option[B] = fa map f
}

implicit def eitherFunctor[Fixed]:Functor[({type lambda[A] = Either[Fixed,A]})#lambda] =
  new Functor[({type lambda[A] = Either[Fixed, A]})#lambda] {
  override def map[A, B](fa: Either[Fixed, A])(f: (A) => B): Either[Fixed, B] = fa map f
}





object Functor {
  def apply[F[_] : Functor]:Functor[F] = implicitly[Functor[F]]
}


Functor[List].map(List(1,2,3))(_*3)
Functor[Option].map(Some(2))(_*3)

// lift example
case class User(id:Int)
def findUser(id:Int):User = User(id)
Functor[List].lift(findUser)   apply List(1,2,3)
Functor[Option].lift(findUser) apply Some(2)

Functor[List].distribute(List("a" -> 1, "b" -> 2))


Functor[List].fproduct(List("a", "b", "c"))(_.charAt(0).toShort).toMap


Functor[({type l[x] = Either[String,x]})#l].map(Right(2))(_*3)


//val x: {type H[X] = (X,String)} =
case class X() {
  type Inner = String
}

val x1 = X()
val x2 = X()
val y1: (X#Inner) = "str"
val y2: x2.Inner = "str"

/***
 * HOMEWORK
 */
// (A,Fixed)
implicit def firstTuple2Functor[Fixed]:Functor[({type l[A] = (A,Fixed)})#l] =
  new Functor[({type l[A] = (A,Fixed)})#l] {
    override def map[A, B](fa: (A, Fixed))(f: A => B): (B, Fixed) = f(fa._1) -> fa._2
  }
Functor[({type l[x] = (x,String)})#l].map(1 -> "str")(_*3) // (3,"str")

// Fixed => A
// Fixed => B
// f: A => B
//implicit def func1Functor[Fixed]:Functor[???] = ???
/// Int=>String,    String->Boolean ==>> Int=>Boolean