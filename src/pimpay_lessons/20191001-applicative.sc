trait Functor[F[_]]{
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
  def apply[F[_] : Functor]:Functor[F] = implicitly[Functor[F]]
}

trait Monad[F[_]] extends Functor[F] {
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def ap[A, B](fa: F[A])(ff: F[A => B]): F[B] = flatMap(fa)(a => map(ff)(f => f(a))) //TODO: flatMap + unit
  override def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => unit(f(a)))
}

trait Applicative[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]
  def ap[A,B](fa:F[A])(ff:F[A => B]): F[B] // apply

  //derivatives
  def map[A,B](fa:F[A])(f: A => B): F[B] = ap(fa)(unit(f)) //TODO: using unit + ap
  def product[A,B](fa: F[A], fb: F[B]): F[(A, B)] = ap(fa)(map(fb)(b => _ -> b)) // ap(fa)(map(fb)(b => ((a:A) => a -> b)))
  def combine[A,B,C](fa:F[A], fb:F[B])(f: (A,B) => C): F[C] = map(product(fa, fb))(f.tupled)

  //ap via product
  //def ap[A,B](fa:F[A])(ff:F[A => B]): F[B] = map(product(fa, ff))(case (a, f) => f(a))

  //for List
  def sequence[A](fas:List[F[A]]): F[List[A]] = traverse(fas)(identity)
  def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldLeft(unit(List.empty[B]))((acc, el) => combine(acc, f(el))(_ :+ _))
}

implicit val optionMonad:Monad[Option] = new Monad[Option]{
  override def flatMap[A, B](fa: Option[A])(f: A => Option[B]) = fa flatMap f

  override def unit[A](a: => A) = Option(a)
}

Functor[Option].map(Some(4))(_ * 2)