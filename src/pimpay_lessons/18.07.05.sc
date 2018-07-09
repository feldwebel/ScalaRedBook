object less180705 {

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(v) => Some(f(v))
    case None => None
  }

  def getOrElse[AA >: A](b: AA): AA = this match {
    case Some(v) => v
    case None => b
  }

  def filter(p: A => Boolean): Option[A] =
    map((i: A) => if (p(i)) this else None).getOrElse(None)

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None

  def filterViaFlatMap(p:A => Boolean): Option[A] =
    flatMap(i => if (p(i)) this else None)

  def orElse[AA >: A](b: => Option[AA]): Option[AA] =
    map(Some(_)).getOrElse(b)
}

case class Some[A](get: A) extends Option[A]
case object None extends Option[Nothing]

def avg(xs:List[Double]):Option[Double] =
  if (xs.isEmpty) None else Some(xs.sum/xs.length)

def variance(xs:List[Double]): Option[Double] =
  avg(xs) flatMap (m => avg(xs map(x => math.pow(x - m, 2))))

avg(List(1, 2, 3, 4, 5))
avg(List())

variance(List(1, 2, 3, 4))
variance(List(1, 77, 88, 3))

case class Employee(name: String, department: String)
def lookupByName(name:String): Option[Employee] =
  if (name == "bob") Some(Employee(name, "IT")) else None

lookupByName("bob")
lookupByName("alice")

  lookupByName("bob")
    .map(_.department)
    .filterViaFlatMap(_ == "accounting")
    .getOrElse("Default")


}