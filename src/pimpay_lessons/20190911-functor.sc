import scala.languageFeature.higherKinds
import scala.languageFeature.reflectiveCalls

trait Functor[F[_]] {
  self =>
  def map[A, B](fa: F[A])(f: A => B): F[B]

  def lift[A, B](f: A => B): F[A] => F[B] = map(_)(f)

  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) = map(fab)(_._1) -> map(fab)(_._2)

  def as[A, B](fa: F[A], b: B): F[B] = map(fa)(_ => b)

  def fproduct[A, B](fa: F[A])(f: A => B): F[(A, B)] = map(fa)(a => a -> f(a))

  // they do compose
  def compose[G[_] : Functor]: Functor[({type l[a] = F[G[a]]})#l] = new Functor[({type l[a] = F[G[a]]})#l] {
    val G = implicitly[Functor[G]]

    override def map[A, B](fa: F[G[A]])(f: (A) => B) = self.map(fa)((ga: G[A]) => G.map(ga)(f))
  }
}

object Functor {
  def apply[F[_] : Functor]: Functor[F] = implicitly[Functor[F]]
}

implicit val listFunctor: Functor[List] = new Functor[List] {
  override def map[A, B](fa: List[A])(f: (A) => B): List[B] = fa map f
}

implicit val optionFunctor: Functor[Option] = new Functor[Option] {
  override def map[A, B](fa: Option[A])(f: (A) => B): Option[B] = fa map f
}

case class User(id: Int, birthYear: Int) {
  def age: Int = 2019 - birthYear
}

val nested: List[Option[User]] = List(Some(User(1, 1989)), None, Some(User(4, 1999)))

// direct composition
val listOptionFunctor1 = listFunctor.compose[Option]
val listOptionFunctor2 = listFunctor.compose(optionFunctor)
val listOptionFunctor3 = Functor[List].compose[Option]

listOptionFunctor1.fproduct(nested)(_.age)
listOptionFunctor2.fproduct(nested)(_.age)
listOptionFunctor3.fproduct(nested)(_.age)

// semi-composed
implicit def composed[F[_] : Functor, G[_] : Functor]: Functor[({type l[a] = F[G[a]]})#l] =
  Functor[F].compose[G]

composed[List, Option].fproduct(nested)(_.age)

// data->nesting
trait Nested[F[_], G[_]] {
  type Aux[a] = F[G[a]]
}

type >>[F[_], G[_]] = Nested[F, G]

//Functor[Nested[List, Option]#Aux].fproduct(nested)(_.age)

// Nested List Option T
Functor[(List >> Option)#Aux].fproduct(nested)(_.age)


//

import scala.concurrent.ExecutionContext
import scala.concurrent.{Future, Await}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

implicit val futureFunctor: Functor[Future] = new Functor[Future] {
  override def map[A, B](fa: Future[A])(f: A => B): Future[B] = fa map f
}

val nested3 = Future(nested)

val r1 = Functor[Future].compose[List].compose[Option].fproduct(nested3)(_.age)
Await.result(r1, Duration.Inf)

val r2 = Functor[Future].compose[(List >> Option)#Aux].fproduct(nested3)(_.age)
Await.result(r2, Duration.Inf)

// full nested
trait Nested3[F[_], G[_], H[_]] {
  type Aux[a] = F[G[H[a]]]
}

Functor[Nested3[Future, List, Option]#Aux].fproduct(nested3)(_.age)

implicit def composed3[F[_] : Functor, G[_] : Functor, H[_] : Functor]: Functor[({type l[a] = F[G[H[a]]]})#l] =
  Functor[F].compose[G].compose[H]

composed3[Future, List, Option].fproduct(nested3)(_.age)

val f = ((u: User) => u.age)
nested3

implicit class FunctionOps[A,B](f: A => B) {
  def applyOnNested[F[_] : Functor](d: F[A]) =
    implicitly[Functor[F]].map(d)(f)

  def applyOnNested[F[_]: Functor, G[_] : Functor](d: F[G[A]]) =
    Functor[(F>>G)#Aux].map(d)(f)

  def applyOnNested[F[_]: Functor, G[_] : Functor, H[_]: Functor](d: F[G[H[A]]]) =
    Functor[((F >> (G>>H)#Aux)#Aux)].map(d)(f)
}

f applyOnNested Option(User(1, 1828))
f applyOnNested nested
val r3 = f applyOnNested nested3
Await.result(r3, Duration.Inf)