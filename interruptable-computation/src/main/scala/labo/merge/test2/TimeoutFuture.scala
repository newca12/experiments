package labo.merge.test2

import scala.concurrent.{Promise, Future}
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import akka.actor.ActorSystem

object TimeoutFuture extends App {

  def test(): Int = {
    println("start test")
    try {
      //while (true) {}
      Thread.sleep(4000)
    } catch {
      case tde: ThreadDeath ⇒ println("DEATH1")
      case _                ⇒ println("SOME EXCEPTION1")
    }
    println("EXITING")
    0
  }

  val f = WithTimeoutFuture(2 second)(test)
  f.onFailure { case ex ⇒ println(ex.getClass) }
  f.onSuccess { case i  ⇒ println(i) }

}

object WithTimeoutFuture {

  def apply[A](timeout: FiniteDuration)(block: ⇒ A): Future[A] = {
    val system  = ActorSystem("system")
    val promise = Promise[A]()

    system.scheduler.scheduleOnce(timeout) {
      println("scheduleOnce")
      //promise tryFailure new java.util.concurrent.TimeoutException
      promise failure new java.util.concurrent.TimeoutException
    }

    Future {
      try {
        promise success block
      } catch {
        case e: Throwable ⇒ println("throw"); promise failure e
      }
    }

    promise.future
  }
}
