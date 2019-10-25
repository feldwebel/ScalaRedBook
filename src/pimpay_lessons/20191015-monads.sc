import scala.concurrent.ExecutionContext
import scala.language.higherKinds
trait Functor[F[_]] { self =>
  def map[A,B](fa:F[A])(f:A=>B):F[B]
  // derivates
  def lift[A, B](f: A => B): F[A] => F[B] = map(_)(f)
  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) = map(fab)(_._1) -> map(fab)(_._2)
  def as[A, B](fa: F[A], b: B): F[B] = map(fa)(_ => b)
  def fproduct[A, B](fa: F[A])(f: A => B): F[(A, B)] = map(fa)(a => a -> f(a))
}

object Functor {
  def apply[F[_] : Functor]:Functor[F] = implicitly[Functor[F]]
}

trait Applicative[F[_]] extends Functor[F] {
  // given
  def unit[A](a: => A):F[A]
  def ap[A,B](fa:F[A])(ff:F[A=>B]):F[B] // apply

  // derivates
  override def map[A,B](fa:F[A])(f:A=>B):F[B] = ap(fa)(unit(f))
  def product[A,B](fa:F[A], fb:F[B]):F[(A,B)] = ap(fa)(map(fb)(b => _ -> b))
  def map2[A,B,C](fa:F[A], fb:F[B])(f: (A,B) => C):F[C] = map(product(fa,fb))(f.tupled)
  def map3[A,B,C,D](fa:F[A], fb:F[B], fc:F[C])(f: (A,B,C) => D):F[D] = map(product(fa, product(fb,fc))) {
    case (a,(b,c)) => f(a,b,c)
  }
  // def map... 21

  def compose[G[_] : Applicative] = Applicative.compose[F,G](this, implicitly[Applicative[G]])


  // for list
  def sequence[A, T[_] : Traverse](fas:T[F[A]]):F[T[A]] =
    implicitly[Traverse[T]].sequence(fas)(this)

  def traverse[A,B, T[_] : Traverse](as:T[A])(f:A=>F[B]):F[T[B]] =
    implicitly[Traverse[T]].traverse(as)(f)(this)

  def replicateM[A](n:Int)(fa:F[A]):F[List[A]] = sequence(List.fill(n)(fa))


  def ifA[A](cond:F[Boolean])(onTrue: => F[A], onFalse: => F[A]):F[A] = map3(cond, onTrue, onFalse)( (c,t,f) => if (c) t else f )
}

object Applicative {
  def apply[F[_] : Applicative]:Applicative[F] = implicitly[Applicative[F]]

  def compose[F[_] : Applicative, G[_] : Applicative]: Applicative[({type l[x] = F[G[x]]})#l] =
    new Applicative[({type l[x] = F[G[x]]})#l] {
      val F = implicitly[Applicative[F]]
      val G = implicitly[Applicative[G]]
      override def unit[A](a: => A):F[G[A]] = F.unit(G.unit(a))
      override def ap[A, B](fa: F[G[A]])(ff: F[G[(A) => B]]):F[G[B]] = F.map2(fa, ff){
        G.ap(_)(_)
      }
    }
}

trait Traverse[T[_]] {
  def traverse[F[_] : Applicative, A, B](tas:T[A])(f: A => F[B]):F[T[B]]
  def sequence[F[_]: Applicative, A](tas:T[F[A]]):F[T[A]] = traverse(tas)(identity)
}

implicit val listTraverse:Traverse[List] = new Traverse[List] {
  override def traverse[F[_] : Applicative, A, B](tas: List[A])(f: A => F[B]):F[List[B]] = {
    val F = implicitly[Applicative[F]]
    tas.foldRight(F.unit(List.empty[B]))( (el, acc) => F.map2(f(el), acc)(_ +: _))
  }
}

implicit val optionTraverse:Traverse[Option] = new Traverse[Option] {
  override def traverse[F[_] : Applicative, A, B](tas: Option[A])(f: A => F[B]):F[Option[B]] = {
    val F = implicitly[Applicative[F]]
    tas.fold(F.unit(Option.empty[B]))(a => F.map(f(a))(Option.apply))
  }
}

implicit def mapTraverse[K]:Traverse[({type l[x] = Map[K,x]})#l] = ???

case class Tree[A](head:A, tail:List[Tree[A]])

implicit val treeTraverse:Traverse[Tree] = ???

trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A,B](fa:F[A])(f:A=>F[B]):F[B]
  def ap[A,B](fa:F[A])(ff:F[A=>B]):F[B] = flatMap(fa)(a => map(ff)(f => f(a)))
  override def map[A,B](fa:F[A])(f: A=>B):F[B] = flatMap(fa)(a => unit(f(a)))

  // derivates
  def flatten[A](ffa:F[F[A]]):F[A] = ???
  def compose[A,B,C](f:A => F[B])(g: B => F[C]):A => F[C] = ???


  def ifM[A](cond:F[Boolean])(onTrue: => F[A], onFalse: => F[A]):F[A] = flatMap(cond)(c => if (c) onTrue else onFalse)

  // homework
  def filterM[A](list:List[A])(f: A => F[Boolean]):F[List[A]] = ???
}

object Monad {
  def apply[F[_] : Monad]:Monad[F] = implicitly[Monad[F]]

  /*
  def compose[F[_] : Monad, G[_] : Monad]:Monad[({type l[x] = F[G[x]]})#l] = new Monad[({type l[x] = F[G[x]]})#l] {
      val F = implicitly[Monad[F]]
      val G = implicitly[Monad[G]]

      override def unit[A](a: => A) = F.unit(G.unit(a))
      override def flatMap[A, B](fa: F[G[A]])(f: A => F[G[B]]) = F.flatMap(fa)(ga => G.flatMap(ga)(a => F.unit(f(a))) )
  }*/
}



implicit val optionMonad:Monad[Option] = new Monad[Option] {
  override def flatMap[A, B](fa: Option[A])(f: (A) => Option[B]) = fa flatMap f
  override def unit[A](a: => A) = Option(a)
}

implicit val listMonad:Monad[List] = new Monad[List] {
  override def flatMap[A, B](fa: List[A])(f: (A) => List[B]): List[B] = fa flatMap f
  override def unit[A](a: => A): List[A] = List(a)
}

case class State[S,A](run:S => (A,S))
// implicit def
// type lambda, where S is fixed

implicit def stateMonad[S]:Monad[({type l[x] = State[S,x]})#l] = new Monad[({type l[x] = State[S, x]})#l] {
  override def flatMap[A, B](fa: State[S, A])(f: (A) => State[S, B]): State[S, B] = State(s => {
    val(a, s2) = fa.run(s)
    f(a).run(s2)
  })
  override def unit[A](a: => A): State[S, A] = State((a,_))
}

// +Either
implicit def rightBiasedEither[E]:Monad[({type l[x] = Either[E,x]})#l] = new Monad[({type l[x] = Either[E, x]})#l] {
  override def flatMap[A, B](fa: Either[E, A])(f: (A) => Either[E, B]): Either[E, B] = fa flatMap f
  override def unit[A](a: => A): Either[E, A] = Right(a)
}

case class Id[+A](v:A)

implicit val identityMonad:Monad[Id] = new Monad[Id] {
  override def flatMap[A, B](fa: Id[A])(f: (A) => Id[B]): Id[B] = f(fa.v)
  override def unit[A](a: => A): Id[A] = Id(a)
}


Applicative[Option].compose[Id].unit(5)


Applicative[Option].sequence(List(Option(2), Option(3)))

Applicative[Id].sequence(Option(Id(2)))