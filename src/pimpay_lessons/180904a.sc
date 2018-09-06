
object lesson180904a {

/*  type State[S, +A] = S => (A, S)

  def unit[A, S](a: A): State[S, A] = s => (a, s)

  def flatMap[A, B, S](r: State[S, A])(f:A => State[S, B]): State[S, B] =
    s => {
      val (a, s1) = r(s)
      f(a)(s1)
    }*/

  case class State[S, +A](run:S => (A,S)) {
    def flatMap[B](f:A => State[S,B]):State[S,B] =
      State(s => {
        val (a, s1) = run(s)
        f(a) run s1})
    def map[B](f:A => B): State[S,B] = flatMap(a => State.unit(f(a)))
  }

  object State {
    def unit[S,A](a:A):State[S,A] = State(s => (a, s))
    def sequence[S,A](l:List[State[S, A]]): State[S, List[A]] =
      l.foldRight(unit[S, List[A]](List.empty[A]))((i, a) => map2(a, i)(_ :+ _))

    def map2[A,B,C,S](as:State[S,A], bs:State[S,B])(f:(A, B) => C): State[S,C] =
      for {
        a <- as
        b <- bs
      } yield f(a, b)

    def get[S]: State[S,S] = State(s => (s, s))
    def set[S](s:S): State[S, Unit] = State(_ => ((), s))
    def modify[S](f:S => S): State[S, Unit] = for {
      s <- get
      _ <- set(f(s))
    } yield ()
  }

  /*
  ********************
  * Домашнее задание *
  ********************

  Чтобы ближе познакомиться со State монадой, реализуйте логику конечного автомата, моделирующего работу автомата с конфетами. https://im0-tub-ru.yandex.net/i?id=0ba77cf14df5dddb297350f926751471-l&n=13
  В авмомат можно либо вставить монетку, либо повернуть рычажок, чтоб получить конфету. Автомат может единовременно находиться в одном из двух состояний: заблокирован или разблокирован. Он также позволяет отслеживать
    количество оставшихся конфет и количество накопленных монет.
    */

  // действия:
  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input

  // состояние:
  case class Machine(locked: Boolean, candies: Int, coins: Int)

  object CandyMachine {
    def work(m: Machine, i: Input): Machine = (m, i) match {
      case (Machine(_, 0, _), _) => m
      case (Machine(true, _, _), Turn) => m
      case (Machine(_, candy, coin), Coin) => Machine(false, candy, coin + 1)
      case (Machine(false, candy, coin), Turn) => Machine(true, candy - 1, coin )
    }
  }

  /*
  Правила работы машины:
    * Вставка монеты в заблокированный автомат, вызовет его разблокировку, при условии наличия конфет
    * Поворот рычажка в разблокированном автомате приведёт к выдаче конфеты и последующей блокировке автомата
    * Поворот рычажка в заблокированном автомате, ровно как и вставка монеты в разблокированный -- ни к чему не приведёт
  * Автомат, у которого кончились конфеты, перестаёт реагировать на действия

  Метод simulateMachine должен применить действия к заданному автомату и вернуть количество монет и конфет, оставшихся в автомате. Например, имея автомат, в котором 10 монет и 5 конфет, с помощью которого успешно "купили"
  4 конфеты, мы должны получить (14,1).
  */


  def simulateMachine(inputs: List[Input], m: Machine)/*: State[Machine, (Int, Int)] */= {
    val result = inputs.foldRight(m)((s, acc) => CandyMachine.work(m, s))
    
  }


  val mach = Machine(false, 10, 0)

  val uuu = CandyMachine.work(mach, Coin)
  val in = List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)

  simulateMachine(in, mach)

}