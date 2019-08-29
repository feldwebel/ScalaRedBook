// Monoid.scala
trait Monoid[A] {
  def z:A
  def op(a:A,b:A):A
}

implicit class MonoidOps[A:Monoid](a:A) {
  def |@|(b:A):A = implicitly[Monoid[A]].op(a,b)
}

// Writer.scala (uses monoid)
case class Writer[W : Monoid, A](run: (W,A)) {
  def flatMap[B](f: A => Writer[W,B]):Writer[W,B] = {
    val (w1,a) = run
    val (w2,b) = f(a).run

    Writer(w1 |@| w2, b)
  }

  def map[B](f: A => B):Writer[W,B] = flatMap(a => f(a).pure)

  def written:W = run._1
  def value:A = run._2

  def reset:Writer[W,A] = Writer(implicitly[Monoid[W]].z, value)

  def mapWritten[WW : Monoid](f: W => WW):Writer[WW,A] = Writer(f(written), value)
}

implicit class WrittenOps[W : Monoid](w:W) {
  def tell:Writer[W,Unit] = Writer(w,())
}

implicit class WriterOps[A](v:A) {
  def writer[W : Monoid](w:W):Writer[W,A] = Writer(w,v)
  def pure[W : Monoid]: Writer[W,A] = Writer(implicitly[Monoid[W]].z,v)
}

// Domain.scala (uses writer + monoid)


implicit def vectorMonoid[A]:Monoid[Vector[A]] = new Monoid[Vector[A]] {
  override def z: Vector[A] = Vector.empty
  override def op(a: Vector[A], b: Vector[A]): Vector[A] = a ++ b
}

case class Log(msg:String, scope:Option[String])
type Logs = Vector[Log]

type Logged[A] = Writer[Logs,A]

def log(msg:String):Logs = Vector(Log(msg,None))
def log(msg:String, scope:String):Logs = Vector(Log(msg,Option(scope)))



case class User(id:Int)
def findUser(id:Int):Logged[User] = User(id) writer log(s"Looked user $id", "findUser")
def isDeleted(u:User):Boolean = true

val program = for {
  user      <- findUser(5).reset mapWritten (_ => log("overriden"))
  _         <- log("sleeping", "for").tell
  isDeleted <- isDeleted(user).pure[Logs]
} yield isDeleted

program.value
program.written