case class Player(name:String, score:Int)

def detectWinner(p1:Player, p2:Player): Option[Player] = {
  if (p1.score > p2.score) Option(p1)
  else if (p2.score > p1.score) Option(p2)
  else Option.empty
}

def winnerMsg(result: Option[Player]): String =  result match {
  case Some(p) => s"Player ${p.name} wins with ${p.score}"
  case None => "draw!"
}

def game(p1:Player, p2:Player):IO[Option[Player]] = for {
  winner <- IO.unit(detectWinner(p1, p2))
  _      <- PrintLn(winnerMsg(winner))
} yield winner


def game1(p1:Player, p2:Player):Unit = {
  if (p1.score > p2.score) println(s"Player #1 ${p1.name} wins")
  else if (p2.score > p1.score) println(s"Player #2 ${p2.name} wins")
  else println("draw!")
}

game(Player("Tom", 0), Player("bob", 10))
game(Player("Tom", 20), Player("bob", 10))
game(Player("Tom", 10), Player("bob", 10))


trait IO[A] { self=>
  def run():A
  //def ++(io2: IO):IO = () => {self.run(); io2.run()}
  def flatMap[B](f: A => IO[B]):IO[B] = () => { f(self.run()). run()}
  def map[B](f: A=>B): IO[B] = flatMap(a => IO.unit(f(a)))
}

object IO {
  def unit[A](a: => A): IO[A] = () => a
  def empty:IO[Unit] = unit(())
}

def PrintLn(s:String): IO[Unit] = () => println(s)

val gm = game(Player("Tom", 10), Player("bob", 10)).run


val tournament = for {
  winner1 <- game(Player("Tom", 20), Player("bob", 10))
  winner2 <- game(Player("Alice", 30), Player("Ann", 10))
  finalist <- game(winner1.get, winner2.get)
} yield finalist

tournament.run
