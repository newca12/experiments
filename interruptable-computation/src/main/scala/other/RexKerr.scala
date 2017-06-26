package other

import scala.concurrent.duration.DurationInt
import scala.concurrent.Await

// This method will not interrupt a non-interruptible computation.
// Surprisingly Try does not catch java.lang.InterruptedException

object RexKerr {

  import scala.util.Try
  import scala.concurrent.Future
  import scala.concurrent._

  //http://stackoverflow.com/a/16616031
  def launch[T](f: () ⇒ T, timeout: Long): Future[Try[T]] = {

    val aref = new java.util.concurrent.atomic.AtomicReference[Thread]()
    val cdl  = new java.util.concurrent.CountDownLatch(1)

    import ExecutionContext.Implicits.global
    Future { Thread.sleep(timeout); cdl.await(); aref.get().interrupt }  // 1
    Future { aref.set(Thread.currentThread); cdl.countDown(); Try(f()) } // 2
  }
}

object Demo extends App {
  try {
    val f = () ⇒ common.Computations.longComputation(true, 20000)
    //val f = () ⇒ common.Computations.fakeComputation(true)
    //val f = () ⇒ common.Computations.brokenComputation()
    val n = RexKerr.launch[Option[Int]](f, 2000)
    println(s"${Await.result(n, 50.seconds).getOrElse("No")} decimal of Pi computed")
  } catch {
    case e: java.lang.InterruptedException ⇒ println("InterruptedException catched")
    case e: java.util.concurrent.TimeoutException ⇒
      println(s"TimeOutException catched => Computation interrupted. ${e.getMessage}")
    case e: Throwable ⇒ println(s"An unexpected exception occcured: ${e}")
  }
  println("Delay to check if the computation has been interrupted or not")
  Thread.sleep(10000)
}
