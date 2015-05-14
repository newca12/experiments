package ambassy

import java.util.concurrent.{ Callable, Executors, Future, TimeUnit, TimeoutException }

import scala.concurrent.duration.{ Duration, DurationInt }

//This method will not interrupt a non-interruptible computation.

object Ambassy extends App {

  //Same method used in Coursera infrastructure
  //Do we wan't Scala Future here ?
  //see also (Waiting for process termination) http://im4java.sourceforge.net/docs/dev-guide.html#parallelProcessing  
  def forkProcess[T](fun: () ⇒ T, timeout: Duration) = {
    val executor = Executors.newSingleThreadExecutor()
    val future: Future[T] = executor.submit(new Callable[T] {
      def call = { fun() }
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

  def computation(): Int = {
    println("Start computation:" + Thread.currentThread() + ":" + Thread.currentThread().getId)
    try {
      //while (true) {} //simulate a non-interruptible computation
      //Thread.sleep(10000) //simulate an interruptible computation
      common.Pi.longComputation(true, 10000)
    } catch {
      case tde: ThreadDeath         ⇒
        println("DEATH1"); 0 //never trigged
      case ie: InterruptedException ⇒
        println("InterruptedException catched => Computation interrupted"); 0 //trigged only if computation is interruptible
      case _: Throwable             ⇒ println("SOME EXCEPTION1"); 0
    } finally {
      println("Computation terminated") //never reached till the end of the computation if the computation is non-interruptible
    }
  }

  //2 possibilities :
  //- we get the value resulting from the computation
  //- we get notified that a timeout occurred but we've got no guarantee about the computation termination.
  try {
    val n = forkProcess[Int](computation, 2.seconds)
    println(s"$n decimal of Pi computed")
  } catch {
    case e: TimeoutException ⇒ println("TimeoutException catched")
  }

}