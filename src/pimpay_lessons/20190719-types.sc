import scala.annotation.implicitNotFound
import scala.reflect.runtime.universe._

// runtime/dynamic poly
trait Animal {
  def voice:String
}

class Dog extends Animal {
  override def voice: String = "wooh"
}

class Cat extends Animal {
  override def voice: String = "meow"
}

def voiceTwice(animal:Animal):String = {
  // RTTI
  animal.voice * 2
}

voiceTwice(new Dog)
voiceTwice(new Cat)

val pets = Seq(new Dog, new Dog, new Cat)

for { pet <- pets } yield voiceTwice(pet)

// parametric poly (compile)
def curry[A,B,C](f: (A,B) => C): A => B => C = a => b => f(a,b)

curry((a:Int,b:Int) => a + b) // curry[Int,Int,Int]
curry((a:String,b:String) => a + b) // curry[String,String,String]

// ad-hoc poly (compile) (type classes)


case class Point(x:Int,y:Int)

// 1. interface
@implicitNotFound("Couldn't find Dumper for type ${A}")
trait Dumper[A] {
  def dump(a:A):String
}

// 2. concrete (ad-hoc) instance(s)
implicit val stringDumper:Dumper[String] = new Dumper[String] {
  override def dump(a: String): String = s"<String(${a.length})>$a"
}

// SAM (Single Abstract Method)
implicit val intDumper:Dumper[Int] = a => s"<Int>$a"

implicit def tuple2Dumper[A : Dumper, B : Dumper]:Dumper[(A,B)] = (t:(A,B)) => {
  val aDumper = implicitly[Dumper[A]]
  val bDumper = implicitly[Dumper[B]]

  s"<Tuple2>{1. ${aDumper.dump(t._1)} 2. ${bDumper.dump(t._2)} }"
}

def summonImplicit[T](implicit e:T):T = e

// 3. summoner
def dump[A](obj:A)(implicit dumper:Dumper[A]):String = dumper.dump(obj)
def dump2[A : Dumper](obj:A):String = { // context bound
  summonImplicit[Dumper[A]].dump(obj) // use implicitly
}

dump("abc")
dump(2)

dump2("abc")
dump2(2)

(2,2)


show { reify { dump((2,2)) } }

dump( ((2,2), (2,2))) // (tuple2Dumper(tuple2Dumper(intDumper,intDumper),tuple2Dumper(intDumper,intDumper)))




implicit def pointDumper(implicit dumper:Dumper[(Int,Int)]):Dumper[Point] = new Dumper[Point] {
  override def dump(a: Point): String = dumper.dump(Point.unapply(a).get)
}

dump(Point(2,2))