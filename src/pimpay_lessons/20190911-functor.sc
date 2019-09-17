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
val listOptionFunctor2 = listFunctor.compose(optionFunctor)m