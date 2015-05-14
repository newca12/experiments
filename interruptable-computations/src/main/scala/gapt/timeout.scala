
package gapt

import scala.concurrent.duration._

//this method from gapt will interrupt a non-interruptible computation by using deprecated Thread.stop

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

object Demo extends App {
  try {
    val n = withTimeout(2.seconds) {
      common.Pi.longComputation(false, 10000)
    }
    println(s"$n decimal of Pi computed")
  } catch {
    case e: TimeOutException ⇒ println("TimeOutException catched => Computation interrupted")
  }
}