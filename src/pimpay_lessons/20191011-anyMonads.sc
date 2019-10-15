trait Monad1[F[_]]{
  def unit[A](a: => A): F[A]
  def flatMap[A,B](fa:F[A])(f:A=>F[B]):F[B]

  def map[A,B](fa:F[A])(f: A=>B):F[B] = flatMap(fa)(fa => unit(f(fa)))
  def join[A](ffa:F[F[A]]): F[A] = flatMap(ffa)(identity)
  def compose[A,B,C](f:A=>F[B])(g:B=>F[C]):A=>F[C] = a => flatMap(f(a))(g)
}

trait Monad2[F[_]]{
  def unit[A](a: => A): F[A]
  def join[A](ffa:F[F[A]]): F[A] //flatten
  def map[A,B](fa:F[A])(f: A=>B):F[B]

  def flatMap[A,B](fa:F[A])(f:A=>F[B]):F[B] = join(map(fa)(f))
  def compose[A,B,C](f:A=>F[B])(g:B=>F[C]):A=>F[C] = a => flatMap(f(a))(g)
}

trait Monad3[F[_]]{
  def unit[A](a: => A): F[A]
  def compose[A,B,C](f:A=>F[B])(g:B=>F[C]):A=>F[C]

  def flatMap[A,B](fa:F[A])(f:A=>F[B]):F[B] = compose((_:Unit) => fa)(f).apply()
  //def flatMap[B,C](fb:F[B])(g:B=>F[C]):F[C] = compose(fb)(g)
}