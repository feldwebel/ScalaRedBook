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

// Option
// List
// Par
// Parser

def combine[A,B,C](a:Option[A], b:Option[B])(f:(A,B)=>C):Option[C] = a flatMap(aa => b map (bb => f(aa, bb)))
def combine[A,B,C](a:List[A], b:List[B])(f:(A,B)=>C):List[C] = a flatMap(aa => b map (bb => f(aa, bb)))
// имплементация независима от типов

trait Applicative[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]
  def ap[A,B](fa:F[A])(ff:F[A => B]): F[B] // apply

  //derivatives
  def map[A,B](fa:F[A])(f: A => B): F[B] = ??? //TODO: using unit + ap
  def product[A,B](fa: F[A], fb: F[B]): F[(A, B)] = ???
  def combine[A,B,C](fa:F[A], fb:F[B])(f: (A,B) => C): F[C] = ???

  //for List
  def sequence[A](fas:List[F[A]]): F[List[A]] = traverse(fas)(identity)
  def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldLeft(unit(List.empty[B]))((acc, el) => combine(acc, f(el))(_ :+ _))
}

trait Monad[F[_]] extends Functor[F]{
  def flatMap[A,B](fa:F[A])(f: A=>F[B]):F[B]
  def ap[A,B](fa: F[A])(ff: F[A=>B]): F[B] = ??? //TODO: flatMap + unit
  /*def unit[A](a: => A):F[A]
  def map[A,B](fa: F[A])(f: A=>B):F[B] = flatMap(fa)(a => unit(f(a)))

  def combine[A,B,C](fa:F[A], fb:F[B])(f: (A,B) => C): F[C] = flatMap(fa)(a => map(fb)(b => f(a,b)))
    //fa flatMap(aa => fb map (bb => f(aa, bb)))

  def sequence[A](fas:List[F[A]]): F[List[A]] = traverse(fas)(identity)
  def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldLeft(unit(List.empty[B]))((acc, el) => combine(acc, f(el))(_ :+ _))*/
}

implicit val optionMonad:Monad[Option] = new Monad[Option]{
  override def flatMap[A, B](fa: Option[A])(f: A => Option[B]) = fa flatMap f

  override def unit[A](a: => A) = Option(a)
}

implicit val listMonad:Monad[List] = new Monad[List]{
  override def flatMap[A, B](fa: List[A])(f: A => List[B]) = fa flatMap f

  override def unit[A](a: => A) = List.empty ++ List(a)
}


case class State[S,A](run:S => (A,S))

trait StateF[Fixed] {
  type l[x] = State[Fixed,x]
}

implicit def stateMonad[S]:Monad[({type lambda[A] = State[S,A]})#lambda] =
  new Monad[({type lambda[A] = State[S,A]})#lambda] {
    override def flatMap[A, B](fa: State[S, A])(f: A => State[S, B]) = State(s => {
      val (a, s1) = fa.run(s)
      f(a) run s1
    })

    override def unit[A](a: => A) = State((a,_))
  }

// Either

implicit def rightBiasedEither[E]:Monad[({type lambda[R] = Either[E,R]})#lambda] =
  new Monad[({type lambda[R] = Either[E,R]})#lambda] {
    override def flatMap[A, B](fa: Either[E, A])(f: A => Either[E, B]) = fa flatMap f

    override def unit[A](a: => A) = Right(a)
  }

case class Id[+A](v:A)

implicit def identityMonad:Monad[Id] = new Monad[Id] {
  override def flatMap[A, B](fa: Id[A])(f: A => Id[B]) = f(fa.v)

  override def unit[A](a: => A) = Id(a)
}

implicit class ForOps[A, F[_]: Monad](fa: F[A]){ //context bound
  val M = implicitly[Monad[F]]
  def map[B](f:A=>B):F[B] = M.map(fa)(f)
  def flatMap[B](f: A => F[B]): F[B] = M.flatMap(fa)(f)
}

val r = for {
  a <- Id(5)
  b <- Id(6)
} yield a + b

r

// List[Par[A]] = Par[List[A]] --sequence
// List[A] -> Future[B] -> List[B]
