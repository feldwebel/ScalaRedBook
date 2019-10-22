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
  def map[A,B](fa:F[A])(f:A=>B):F[B] = ap(fa)(unit(f))
  def product[A,B](fa:F[A], fb:F[B]):F[(A,B)] = ap(fa)(map(fb)(b => _ -> b))
  def map2[A,B,C](fa:F[A], fb:F[B])(f: (A,B) => C):F[C] = map(product(fa,fb))(f.tupled) //combine
  def map3[A,B,C,D](fa:F[A], fb:F[B], fc:F[C])(f:(A,B,C) => D):F[D] =
    map(product(fa, product(fb, fc))){ case (a, (b, c)) => f(a, b, c)}

  //def compose[G[_]: Applicative] = Applicative.compose[F,G]

  // for list
  def sequence[A](fas:List[F[A]]):F[List[A]] = traverse(fas)(identity)
  def traverse[A,B](as:List[A])(f:A=>F[B]):F[List[B]] =
    as.foldLeft(unit(List.empty[B]))( (acc, el) => map2(acc, f(el))(_ :+ _))

  def replicateM[A](n:Int)(fa:F[A]):F[List[A]] = sequence(List.fill(n)(fa))


  def ifA[A](cond:F[Boolean])(onTrue: => F[A], onFalse: => F[A]):F[A] = map(product(cond, product(onTrue, onFalse))) {
    case (c, (t, f)) => if (c) t else f
  }
  ///map(cond, onTrue, onFalse)((c,t,f) => if (c) t else f)
}

object Applicative{
  def compose[F[_]: Applicative, G[_]: Applicative]: Applicative[({type l[x] = F[G[x]]})#l] =
    new Applicative[({type l[x] = F[G[x]]})#l] {
      override def unit[A](a: => A): F[G[A]] = ???

      override def ap[A, B](fa: F[G[A]])(ff: F[G[A => B]]):F[G[B]] = ???
    }
}

trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A,B](fa:F[A])(f:A=>F[B]):F[B]
  def ap[A,B](fa:F[A])(ff:F[A=>B]):F[B] = flatMap(fa)(a => map(ff)(f => f(a)))
  override def map[A,B](fa:F[A])(f: A=>B):F[B] = flatMap(fa)(a => unit(f(a)))

  // derivates
  def flatten[A](ffa:F[F[A]]):F[A] = ???
  def compose[A,B,C](f:A => F[B])(g: B => F[C]):A => F[C] = ???


  def ifM[A](cond:F[Boolean])(onTrue: => F[A], onFalse: => F[A]):F[A] = flatMap(cond)(c => if (c) onTrue else onFalse)
  def filterM[A](list:List[A])(f: A => F[Boolean]):F[List[A]] = ???
}

object Monad {
  def apply[F[_] : Monad]:Monad[F] = implicitly[Monad[F]]
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

// map, flatMap
implicit class forOps[A, F[_] : Monad](fa:F[A]) {
  val M = implicitly[Monad[F]]
  def map[B](f:A => B):F[B] = M.map(fa)(f)
  def flatMap[B](f: A => F[B]):F[B] = M.flatMap(fa)(f)
}


val r = for {
  a <- Id(5)
  b <- Id(6)
} yield a + b


case class Reader[R,A](run:R=>A)

implicit def readerMonad[R]:Monad[({type l[x] = Reader[R,x]})#l] = new Monad[({type l[x] = Reader[R, x]})#l] {
  override def unit[A](a: => A) = Reader(_ => a)
  override def flatMap[A, B](fa: Reader[R, A])(f: (A) => Reader[R, B]) = Reader(r => {
    val a = fa.run(r)
    f(a).run(r)
  })
}



implicit class MonadOps[A](v: => A) {
  def pure[F[_] : Monad]:F[A] = implicitly[Monad[F]].unit(v)
}


case class DbConfig(host:String)
case class AppConfig(logLevel:String, db:DbConfig)
// AppConfig :: String x DbConfig

type AppReader[A] = Reader[AppConfig, A]




object Reader {
  sealed case class AskPartial[R]() {
    def apply[RR](f: R=>RR):Reader[R,RR] = Reader(f(_))
  }

  def ask[R]:Reader[R,R] = Reader(identity)
  def askS[R]: AskPartial[R] = AskPartial[R]
}

def bootstrap(app: AppConfig): Unit = ()

def saveToDb(app: AppConfig, input: String): Boolean = true

def dologic(input: String): AppReader[String] = (input * 2).pure[AppReader]


implicit class ReaderFunctionOps[R,A](f:R=>A) {
  def asReader:Reader[R,A] = Reader(f)
}


def program:AppReader[String] = for {
  _     <- (bootstrap _).asReader
  input <- "yolo".pure[AppReader]
  db    <- Reader.askS[AppConfig](_.db)
  res   <- (saveToDb(_:AppConfig, input)).asReader
} yield db.host

val res = program.run(AppConfig("warn", DbConfig("localhost")))
res

2.pure[Option]
2.pure[List]

Monad[Option].ifM(None)(Option(1), None)
Monad[List].ifM(List(true, false, true))(List("a", "b", "c"), List("z"))

Functor[Option].map(Some(4))(_*2)