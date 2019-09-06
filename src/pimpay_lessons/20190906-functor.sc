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

object Functor {
  def apply[F[_] : Functor]:Functor[F] = implicitly[Functor[F]]
}

case class Id[A](v:A)

implicit val idFunctor:Functor[Id] = new Functor[Id]{
  override def map[A, B](fa: Id[A])(f: A => B) = Id(f(fa.v))
}

implicit class IdOps[A, B](id: Id[A]) {
  val func = implicitly[Functor[Id]]
  def map(f: A => B): Id[B] = func.map(id)(f)
}

Id(5).map(_*5)

import scala.reflect.runtime.universe._
show { reify {
  for (i <- Id(5)) yield i * 5
}}