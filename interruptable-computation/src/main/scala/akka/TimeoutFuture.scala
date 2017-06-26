package akka

import scala.concurrent.{Promise, Future}
import scala.concurrent.duration.FiniteDuration
import akka.actor.ActorSystem
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.Await

//This method will not interrupt a non-interruptible computation.

//http://stackoverflow.com/a/24752581
object TimeoutFuture {

  val actorSystem = ActorSystem("myActorSystem")
  def apply[A](timeout: FiniteDuration)(block: ⇒ A): Future[A] = {
    val promise = Promise[A]()
    actorSystem.scheduler.scheduleOnce(timeout) {
      promise tryFailure new java.util.concurrent.TimeoutException
      ()
    }

    Future {
      try {
        promise success block
      } catch {
        case e: Throwable ⇒ promise failure e
      }
    }

    promise.future
  }
}

object Demo extends App {
  println("Start computation")
  try {
    val n = TimeoutFuture(2.seconds) {
      //common.Computations.longComputation(true, 20000)
      //common.Computations.fakeComputation(true)
      common.Computations.brokenComputation()
    }
    println(s"${Await.result(n, 5.seconds).getOrElse("No")} decimal of Pi computed")
  } catch {
    case to: java.util.concurrent.TimeoutException ⇒
      println(s"TimeOutException catched => Computation interrupted. ${to.getMessage}")
    case e: Throwable ⇒ println(s"An unexpected exception occcured: ${e}")
  }
  println("Shutdown actor system")
  TimeoutFuture.actorSystem.terminate
  Await.result(TimeoutFuture.actorSystem.whenTerminated, 10 seconds)
  println("Actor system has been shutdown")
  println("Delay to check if the computation has been interrupted or not")
  Thread.sleep(10000)
}
