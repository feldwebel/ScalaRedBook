object lesson2 {

  def mul(a: Int, b: Int) = a * b

  implicit class MyRichIntOps(x:Int) {
    def mul(b: Int) = x * b

    def **(pow: Int) = Math.pow(x, pow)
    //def pow(pow:Int): Int = **.toInt()
  }

  5.mul(10)
  mul(5, 10)
  5 mul 10

  2**10

  new MyRichIntOps(2).**(10)
  //new MyRichIntOps(2).pow(10)

/*  class Pet(name:String) {
    println(name)

    override def toString: String = s">>$name^"
    def hello:String = s"#####($name)dfdfdfd"
    def greet(master: Human): String = s"hello ($master.name)"
  }

  class Human(name:String) {
    val name = name
  }

  val a = new Pet("Sharik")
  val b = new Human("Bob")

  a.toString
  a.hello
  a greet b*/

/*  trait LikeFood[F] {
    def food(food: F): String
  }

  trait Food
  class Snickers extends Food
  class KitiKat extends Food

  trait HasName {
    def name: String
    override def toString: String = s"$name"
  }

  class Human(val name:String) extends HasName(name) with LikeFood(Snickers)
  {
    def food: String = "zhopa"
  }


  class Pet(val name:String) extends HasName(name)
    with LikeFood(KitiKat)
  {
    def greet(master:Human):String = s"Hello, my master ${master.name}, my name is $name"
    def food: String = "kuku"
  }*/

  class Pet(val name: String)

  object Pet{
    def apply(name: String) = new Pet(name)
    def default = new Pet("DEFAULT")
  }

  val a = Pet("zhopa")
  a.name
  val b = new Pet("[eq")
  b.name
  val c = Pet.default
  c.name

  class squarer extends (Int => Int) {
    def apply(v: Int) = v * v
  }

  val sq = new squarer

  List(1, 2, 3).map(sq)
}