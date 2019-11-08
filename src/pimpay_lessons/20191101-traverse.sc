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
  //def sequence[A, T[_]: Traverse](fas:T[F[A]]):F[T[A]] = implicitly[Traverse[T]].sequence(fas)(this)
  //def traverse[A,B, T[_]: Traverse](as:List[A])(f:A=>F[B]):F[List[B]] = implicitly[Traverse[T]].traverse(as)(f)(this)
  // as.foldLeft(unit(List.empty[B]))( (acc, el) => map2(acc, f(el))(_ :+ _))

  //def replicateM[A](n:Int)(fa:F[A]):F[List[A]] = sequence(List.fill(n)(fa))


  def ifA[A](cond:F[Boolean])(onTrue: => F[A], onFalse: => F[A]):F[A] = map3(cond, onTrue, onFalse)( (c,t,f) => if (c) t else f )
}

object Applicative {
  def apply[F[_] : Applicative]: Applicative[F] = implicitly[Applicative[F]]
  def compose[F[_] : Applicative, G[_] : Applicative]: Applicative[({type l[x] = F[G[x]]})#l] =
    new Applicative[({type l[x] = F[G[x]]})#l] {
      val F = implicitly[Applicative[F]]
      val G = implicitly[Applicative[G]]
      override def unit[A](a: => A):F[G[A]] = F.unit(G.unit(a))
      override def ap[A, B](fa: F[G[A]])(ff: F[G[(A) => B]]):F[G[B]] = F.map2(fa, ff) {
        case (ga, gf) => G.ap(ga)(gf)
      }
    }
}

implicit val optionMonad:Monad[Option] = new Monad[Option] {
  override def flatMap[A, B](fa: Option[A])(f: (A) => Option[B]) = fa flatMap f
  override def unit[A](a: => A) = Option(a)
}


case class State[S,A](run:S => (A,S))
object State{
  def get[S]: State[S,S] = State(run = s => (s,s))
  def set[S](f: (S) => S):State[S, Unit] = State(run = s => ((), f(s)))
}

trait Monoid[A] {
  def zero:A
  def op(a:A,b:A):A
}

val addMonoid = new Monoid[Int] {
  override def zero = 0
  override def op(a:Int,b:Int):Int = a + b
}

type Const[M, A] = M
def monoidApplicative[M](M:Monoid[M]):Applicative[({type l[x] = Const[M,x]})#l] = new Applicative[({type l[x] = Const[M, x]})#l] {
  override def unit[A](a: => A):M = M.zero
  override def ap[A, B](fa: M)(ff: M):M = M.op(fa,ff)
}

def constApplicative[C](const: C):Applicative[({type l[x] = Const[C, x]})#l] = new Applicative[({type l[x] = Const[C, x]})#l]{
  override def unit[A](a: => A):C = const
  override def ap[A, B](fa: Const[C, A])(ff: Const[C, A => B]): C = const
}
implicit val meaningOfLife = constApplicative(42)

trait Traverse[T[_]] extends Functor[T] {
  // required
  def traverse[F[_] : Applicative, A, B](tas: T[A])(f: A => F[B]): F[T[B]]

  // derives
  def sequence[F[_] : Applicative, A](tas: T[F[A]]): F[T[A]] = traverse(tas)(identity)

  //concrete F = Any
  def map[A, B](tas: T[A])(f: A => B): T[B] = traverse[Option,A,B](tas)(a => Option(f(a)))(optionMonad).get

  //concrete F = Const + Monoid
  def foldMap[M,A](tas: T[A])(f: A => M)(m:Monoid[M]): M =
    traverse[({type l[x] = Const[M,x]})#l,A,Nothing](tas)(f)(monoidApplicative(m))

  //concreteF = State
  def traverseS[S,A,B](tas:T[A])(f:A => State[S,B]): State[S, T[B]] =
    traverse(tas)(f)

  def zipWithIndex[A](tas:T[A]):T[(A,Int)] = traverseS(tas)((a: A) => for {
    i <- State.get[Int]
    _ <- State.set[Int](k => k + 1)
  } yield (a, i) ).run(0)._1


  def toList[A](tas:T[A]):List[A] = traverseS(tas)((a: A) => for {
    l <-State.get[List[A]]
    _ <- State.set[List[A]](l => a :: l)
  } yield ()).run(List.empty[A])._2.reverse

  // homework
  def mapAccum[S,A,B](tas:T[A])(z:S)(f: (A,S) => (B,S)):(T[B], S) = traverseS(tas)((a: A) => for {
    s <- State.get[S]
    (b, s2) = f(a, s)
    _ <- State.set[S](_ => s2)
  } yield b ).run(z) // via traverseS
  // + implement zipWithIndex, and toList via mapAccum
  def zipWithIndex1[A](tas:T[A]):T[(A,Int)] = mapAccum(tas)(0)((a, i) => (a ->i, i+1))._1

  def foldMap2[M,A](tas: T[A])(f: A => M)(m:Monoid[M]): M =
    mapAccum(tas)(m.zero)((a, s) => () -> m.op(f(a), s))._2


  def compose[H[_]: Traverse]: Traverse[({type l[x] = T[H[x]]})#l] = Traverse.compose(this, implicitly[Traverse[H]])

}

object Traverse {
  def compose[F[_] : Traverse, G[_]: Traverse]: Traverse[({type l[x] = F[G[x]]})#l] =
    new Traverse[({type l[x] =  F[G[x]]})#l] {
      val F = implicitly[Traverse[F]]
      val G = implicitly[Traverse[G]]
      override def traverse[AP[_] : Applicative, A, B](tas: F[G[A]])(f: A =>AP[B]):AP[F[G[B]]] = {
        F.traverse(tas)( ga => G.traverse(ga)(f))
      }
    }
}



implicit val listTraverse: Traverse[List] = new Traverse[List] {
  override def traverse[F[_] : Applicative, A, B](tas: List[A])(f: (A) => F[B]):F[List[B]] = {
    val F = implicitly[Applicative[F]]
    tas.foldRight(F.unit(List.empty[B]))( (el, acc) => F.map2(f(el), acc)(_ +: _))
  }
}

implicit val optionTraverse: Traverse[Option] = new Traverse[Option] {
  override def traverse[F[_] : Applicative, A, B](tas: Option[A])(f: (A) => F[B]): F[Option[B]] = {
    val F = implicitly[Applicative[F]]
    tas.fold(F.unit(Option.empty[B]))(a => F.map(f(a))(Option.apply))
  }
}


implicit def mapTraverse[K]: Traverse[({type l[x] = Map[K, x]})#l] = new Traverse[({type l[x] = Map[K, x]})#l] {
  override def traverse[F[_] : Applicative, A, B](tas: Map[K, A])(f: (A) => F[B]):F[Map[K,B]] = {
    val F = implicitly[Applicative[F]]
    val list = listTraverse.traverse(tas.toList){ case (k,v) => F.product(F.unit(k), f(v)) }(F)
    F.map(list)(_.toMap)
  }
}

case class Tree[A](head: A, tail: List[Tree[A]])

implicit val treeTraverse: Traverse[Tree] = new Traverse[Tree] {
  override def traverse[F[_] : Applicative, A, B](tas: Tree[A])(f: (A) => F[B]):F[Tree[B]] = {
    val F = implicitly[Applicative[F]]

    val bHead = f(tas.head)
    val bTail = listTraverse.traverse(tas.tail)(ta => treeTraverse.traverse(ta)(f)(F))(F)

    F.map2(bHead, bTail)(Tree.apply)
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

  // homework
  def filterM[A](list:List[A])(f: A => F[Boolean]):F[List[A]] = ???
}

object Monad {
  def apply[F[_] : Monad]:Monad[F] = implicitly[Monad[F]]

  /*
  def compose[F[_] : Monad, G[_] : Monad]: Monad[({type l[x] = F[G[x]]})#l] = new Monad[({type l[x] = F[G[x]]})#l] {
    val F = implicitly[Applicative[F]]
    val G = implicitly[Applicative[G]]
    override def flatMap[A, B](fa: F[G[A]])(f: (A) => F[G[B]]) = F.flatMap(fa)(ga => G.flatMap(ga)(a => F.unit(f(a))))
    override def unit[A](a: => A) = F.unit(G.unit(a))
  }
  */
}



implicit val listMonad:Monad[List] = new Monad[List] {
  override def flatMap[A, B](fa: List[A])(f: (A) => List[B]): List[B] = fa flatMap f
  override def unit[A](a: => A): List[A] = List(a)
}


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





implicit class MonadOps[A](v: => A) {
  def pure[F[_] : Monad]:F[A] = implicitly[Monad[F]].unit(v)
}


listTraverse.foldMap(List(1,2,3))(identity)(addMonoid)

optionTraverse.foldMap(Option(2))(identity)(addMonoid)
optionTraverse.foldMap(Option.empty[Int])(identity)(addMonoid)

treeTraverse.foldMap(Tree(2, List(Tree(2, Nil) )))(identity)(addMonoid)


listTraverse.sequence[({type l[x] = Const[Int, x]})#l, Int](List(meaningOfLife.unit(1)))
val tree = Tree("a", List(Tree("b", Nil)))

val tree1:Tree[Option[String]] = Tree(Option("a"), List(Tree(Option("b"), Nil)))

treeTraverse.zipWithIndex1(tree)

treeTraverse.toList(tree)

(treeTraverse compose optionTraverse).toList(tree1)