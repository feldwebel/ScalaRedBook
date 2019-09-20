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

trait Monad[F[_]] extends Functor[F]{
  def flatMap[A,B](fa:F[A])(f: A=>F[B]):F[B]
  def unit[A](a: => A):F[A]
  def map[A,B](fa: F[A])(f: A=>B):F[B] = flatMap(fa)(a => unit(f(a)))

  def combine[A,B,C](fa:F[A], fb:F[B])(f: (A,B) => C): F[C] = flatMap(fa)(a => map(fb)(b => f(a,b)))
    //fa flatMap(aa => fb map (bb => f(aa, bb)))
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

implicit def stateMonad[S]:Monad[({type lambda[A] = State[S,A]})#lambda] =
  new Monad[({type lambda[A] = State[S,A]})#lambda] {
    override def flatMap[A, B](fa: State[S, A])(f: A => State[S, B]) = State(s => {
      val (a, s1) = run(fa)
      f(a) run s1
    })

    override def unit[A](a: => A) = State(s => (a,s))
  }

// Either

implicit def rightBiasedEither[E] ???s22222222@i