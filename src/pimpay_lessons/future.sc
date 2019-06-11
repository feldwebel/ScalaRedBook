import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration


// algebra for parallel
def long(w:Int):Future[Int] = Future {
  Thread.sleep(w * 1000)
  w
}

val f2 = for {
  //i <- long(5)
  //g <- long(7
  (i, g) <- long(5) zip long(7)
} yield i + g


// end of universe
val r = Await.result(f2, Duration.Inf)