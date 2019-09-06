//path-dependent types
//structured types
//type projection

//({type l[x] = Either[x, Fixed]})#l
_root_.scala.Int

trait Serializable

trait Application {
  type Config //<: Serializable
  def run(c:Config):Unit ={}
}

object MyApp extends Application {
  type Config = String
}
object MyApp1 extends Application {
  type Config = Int
}

MyApp.run("s")
MyApp1.run(2)

type SmthWithInt = {def f:Int; def g:String; type S}

trait F {
  def f:Int = 2
}

trait G {
  def g: String = "g"
}

trait S{
  type S = Int
}

def stupid (a: {def f: Int; def g: String; type S}): Int = 42

stupid (new F with G with S)

class Network {
  class Node
  def createNode: Node = new Node()
  def registerNode(n:Network#Node):Unit = {}
}

val pimpay = new Network
val google = new Network
pimpay.registerNode(pimpay.createNode)
google.registerNode(google.createNode)

pimpay.registerNode(google.createNode) //Network#Node

trait EitherF[F] {
  type l[A] = Either[F, A]
}

val f: EitherF[String]#l[Int] = Right(2)
val f2: (EitherF[String]#l)[Int] = Right(2)