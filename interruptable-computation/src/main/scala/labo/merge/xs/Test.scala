package labo.merge.xs

import scala.concurrent.Await
import scala.concurrent.duration._

object Test extends App {
  import concurrent.ExecutionContext.Implicits.global

  def test(): Int = {
    println("test:" + Thread.currentThread() + ":" + Thread.currentThread().getId)
    try {
      //while (true) {}
      Thread.sleep(40000)
    } catch {
      case tde: ThreadDeath         ⇒ println("DEATH1")
      case ie: InterruptedException ⇒ println("INTERRUPTED")
      case _ @e                     ⇒ println("SOME EXCEPTION1" + e)
    }
    println("EXITING")
    0
  }

  val e1 = InterruptableFuture.future { test() }
  try {
    val res = Await.result(e1.future, 2 second)
  } catch {
    case ie: java.util.concurrent.TimeoutException ⇒
      println("TIMEOUT"); e1.cancel()
    case _ ⇒ println("SOME EXCEPTION2")
  }

  Thread.sleep(7000)
}
