package ambassy

import java.util.concurrent._

import scala.concurrent.duration.{Duration, DurationInt}

//This method will not interrupt a non-interruptible computation.

object Ambassy extends App {

  //Same method used in Coursera infrastructure
  //Do we wan't Scala Future here ?
  //see also (Waiting for process termination) http://im4java.sourceforge.net/docs/dev-guide.html#parallelProcessing
  def forkProcess[T](fun: () ⇒ T, timeout: Duration) = {
    val executor = Executors.newSingleThreadExecutor()
    val future: Future[T] = executor.submit(new Callable[T] {
      def call = {
        fun()
      }
    })
    try {
      future.get(timeout.toSeconds, TimeUnit.SECONDS)
    } catch {
      case to: TimeoutException ⇒
        future.cancel(true)
        throw to
    } finally {
      /* See API
       * There are no guarantees beyond best-effort attempts to stop
       * processing actively executing tasks.  For example, typical
       * implementations will cancel via Thread#interrupt, so any
       * task that fails to respond to interrupts may never terminate.
       */
      executor.shutdown()
    }
  }

  //2 possibilities :
  //- we get the value resulting from the computation
  //- we get notified that a timeout occurred but we've got no guarantee about the computation termination.
  try {
    val f = () ⇒ common.Computations.longComputation(true, 200)
    //val f = () ⇒ common.Computations.fakeComputation(false)
    //val f = () ⇒ common.Computations.brokenComputation()
    val n = forkProcess[Option[Int]](f, 2.seconds)
    println(s"${n.getOrElse("No")} decimal of Pi computed")
  } catch {
    case e: TimeoutException ⇒ println("TimeoutException catched")
    case e: Throwable        ⇒ println(s"An unexpected exception occcured: ${e}")
  }
  println("Delay to check if the computation has been interrupted or not")
  Thread.sleep(10000)
}
