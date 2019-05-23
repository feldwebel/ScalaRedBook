
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

object lesson180918 {

  implicit val ec = scala.concurrent.ExecutionContext.global

  def ping(sec:Int):Future[Int] = Future {
    val url = new java.net.URL(s"http://api.pplocal.com/test/ping?t=$sec")
    val response = scala.io.Source.fromURL(url).mkString
    response.toInt
  }

  def getServers: Future[Seq[Int]] = {
    Future{
      Thread.sleep(1500)
      Seq(1, 2, 3, 4)
    }

  }

  val avg = for {
    serverList <- getServers
    sequence = serverList map ping
    pings <- Future.sequence(sequence)
  } yield pings.sum.toDouble / pings.length

  val servers = Seq(1, 4, 3, 4, 5)

  val pings = servers map ping

  val future = Future.sequence(pings) map (l => l.sum.toDouble / l.length)

  Await.result(future, Duration.Inf)
  Await.result(avg, Duration.Inf)
  println("KUKU")

}