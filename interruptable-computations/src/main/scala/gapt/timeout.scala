
package gapt

import scala.concurrent.duration._

//withTimeout from gapt will interrupt a non-interruptible computation by using deprecated Thread.stop
//withTimeout2 is compatible but will not interrupt a non-interruptible computation

class TimeOutException(cause: Throwable, val duration: Duration)
  extends Exception(s"Timeout of $duration exceeded.", cause)

/**
 * runs f with timeout to
 *
 * If f does terminate within to milliseconds returns its result. If not
 * throw a TimeOutException. If f throws an exception it is propagated to
 * the caller of withTimeout.
 *
 * Use this as follows:
 * try { withTimeout( 1234 ) {
 *   ... your code ...
 * } } catch {
 *   case e: TimeOutException ...
 *   case ... other exception
 * }
 */
object withTimeout {

  def apply[T](duration: Duration)(f: ⇒ T): T = {
    var result: Either[Throwable, T] = Left(new TimeOutException(null, duration))

    val t = new Thread {
      override def run() = {
        result = try Right(f) catch {
          case e: ThreadDeath ⇒ Left(new TimeOutException(e, duration))
          case t: Throwable   ⇒ Left(t)
        }
      }
    }

    t.start()
    t.join(duration toMillis)
    t.stop()

    // wait until variable result has been written
    t.join()

    result match {
      case Left(t)      ⇒ throw t
      case Right(value) ⇒ value
    }
  }
}

import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent._
import ExecutionContext.Implicits.global

object withTimeout2 {
  def apply[T](to: Duration)(f: ⇒ T): T = {
    Await.result(Future { f }, to)
  }
}

object Demo extends App {
  println("Start computation")
  try {
    val n = withTimeout(2.seconds) {
      common.Computations.longComputation(true, 20000)
      //common.Computations.fakeComputation(true)
    }
    println(s"${n.getOrElse("No")} decimal of Pi computed")
  } catch {
    case e1: TimeOutException                      ⇒ println("TimeOutException catched => Computation interrupted")
    case e2: java.util.concurrent.TimeoutException ⇒ println(s"TimeOutException catched => Computation interrupted. ${e2.getMessage}")
  }
  println("Delay to check if the computation has been interrupted or not")
  Thread.sleep(10000)
}
