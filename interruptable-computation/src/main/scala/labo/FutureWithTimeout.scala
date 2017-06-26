package labo

import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global
import scala.util.{Failure, Success}
import akka.actor.ActorSystem
import akka.pattern.after

//https://nami.me/2015/01/20/scala-futures-with-timeout/
object FutureWithTimeout extends App {

  val system = ActorSystem("theSystem")

  lazy val f = Future {
    Thread.sleep(2000); true
  }
  lazy val t =
    after(duration = 1 second, using = system.scheduler)(Future.failed(new TimeoutException("Future timed out!")))

  val fWithTimeout = Future firstCompletedOf Seq(f, t)

  fWithTimeout.onComplete {
    case Success(x)     => println(x)
    case Failure(error) => println(error)
  }

  Thread.sleep(3000)
  system.terminate()
  Await.result(system.whenTerminated, 3 second)
}
