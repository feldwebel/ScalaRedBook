
object less180904 {

  trait RNG {
    def nextInt: (Int, RNG)
  }

  type State[S, +A] = S => (A, S)
  type Rand[+A] = State[RNG, A]

  def unit[A, S](a: A): State[S, A] = s => (a, s)

  def flatMap[A, B, S](r: State[S, A])(f:A => State[S, B]): State[S, B] =
    s => {
      val (a, s1) = r(s)
      f(a)(s1)
    }

  def map[A,B,S](r:State[S,A])(f: A => B): State[S,B] =
    flatMap(r)(a => unit(f(a)))

  def map2[A,B,C,S](ra:State[S,A], rb:State[S,B])(f:(A, B) => C): State[S,C] =
    flatMap(ra)(a => flatMap(rb)(b => unit(f(a, b))))

  def sequence[A,S](l:List[State[S,A]]): State[S,List[A]] =
    l.foldRight(unit(List.empty[A]))((i, a) => map2(a, i)(_ :+ _))





}