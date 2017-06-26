package labo.merge.test

import scala.concurrent.duration._
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.Left
import scala.Right
import java.util.concurrent.atomic.AtomicReference
import scala.util.Try

object TmpLabs extends App {

  println("gapt:" + Thread.currentThread() + ":" + Thread.currentThread().getId)

  def launch(f: () ⇒ Unit, timeout: Int): Future[Try[Unit]] = {

    val aref = new java.util.concurrent.atomic.AtomicReference[Thread]()
    val cdl  = new java.util.concurrent.CountDownLatch(1)

    import ExecutionContext.Implicits.global
    Future { Thread.sleep(timeout); cdl.await(); aref.get().interrupt }  // 1
    Future { aref.set(Thread.currentThread); cdl.countDown(); Try(f()) } // 2
  }

  def interruptableFuture[T](fun: () ⇒ T) /*(implicit ex: ExecutionContext)*/: (Future[T], () ⇒ Boolean) = {
    val p    = Promise[T]()
    val f    = p.future
    val aref = new AtomicReference[Thread](null)
    p tryCompleteWith Future {
      val thread = Thread.currentThread
      aref.synchronized { aref.set(thread) }
      try fun()
      finally {
        val wasInterrupted = (aref.synchronized { aref getAndSet null }) ne thread
        //Deal with interrupted flag of this thread in desired
      }
    }

    (f, () ⇒ {
      aref.synchronized { Option(aref getAndSet null) foreach { _.interrupt() } }
      p.tryFailure(new CancellationException)
    })
  }

  def interruptableFutureV[T](fun: Future[T] ⇒ T) /*(implicit ex: ExecutionContext)*/: (Future[T], () ⇒ Boolean) = {
    val p                     = Promise[T]()
    val f                     = p.future
    val lock                  = new Object
    var currentThread: Thread = null
    def updateCurrentThread(newThread: Thread): Thread = {
      val old = currentThread
      currentThread = newThread
      old
    }
    p tryCompleteWith Future {
      val thread = Thread.currentThread
      println("iFV1:" + thread + ":" + thread.getId)
      lock.synchronized { updateCurrentThread(thread) }
      try fun(f)
      finally {
        val wasInterrupted = lock.synchronized { updateCurrentThread(null) } ne thread
        //Deal with interrupted flag of this thread in desired
        println("iFV2:" + thread + ":" + thread.getId)
        //thread.interrupt()

      }
    }

    (f,
     () ⇒
       lock.synchronized {
         Option(updateCurrentThread(null)) exists { t ⇒
           t.interrupt()
           p.tryFailure(new CancellationException)
         }
     })
  }

  def test(): Int = {
    println("test:" + Thread.currentThread() + ":" + Thread.currentThread().getId)
    try {
      while (true) {}
      //Thread.sleep(4000)
    } catch {
      case tde: ThreadDeath         ⇒ println("DEATH1")
      case ie: InterruptedException ⇒ println("INTERRUPTED")
      case _                        ⇒ println("SOME EXCEPTION1")
    }
    println("EXITING")
    0
  }

  def test2(): Int = {
    println("test2")
    while (!Thread.currentThread.isInterrupted) {
      try {
        while (true) {}
      } catch {
        //case e: ZMQException if ZMQ.Error.ETERM.getCode == e.getErrorCode => Thread.currentThread.interrupt()
        case e ⇒ throw e
      }
    }
    println("EXITING")
    0
  }

  //Rex OK with Thread.sleep KO with while(true)
  /*  try {
    val f = launch(test, 3000)
    f.onFailure { case ex => println(ex.getClass) }
    f.onSuccess { case i => println(i) }
  } catch {
    case _ => println("SOME EXCEPTION2")
  }
  Thread.sleep(6000)*/

  //Vic
  /*    val (f, cancel) = interruptableFutureV[Int](future => {
  while(!future.isCompleted) //test // isCompleted acts as our interrupted-flag
    println("test:"+Thread.currentThread()+":"+Thread.currentThread().getId)
    try {
      while (true) {}
      //Thread.sleep(4000)
    } catch {
      case tde: ThreadDeath => println("DEATH1")
      case ie: InterruptedException => println("INTERRUPTED")
      case _                => println("SOME EXCEPTION1")
    }
    println("EXITING")
    0
  12  // when we're done, return some result
})

  Thread.sleep(7000)
  val wasCancelled = cancel()
  println("wasCancelled: " + wasCancelled)*/
  /*  f.onFailure { case ex => println(ex.getClass) }
  f.onSuccess { case i => println(i) }*/
  Thread.sleep(6000)

  //non Vic
  //interruptableFuture(test)
  /*    val (f, cancel) = interruptableFuture[Int] { () => test()
    val latch = new CountDownLatch(1)

    latch.await(5, TimeUnit.SECONDS)    // Blocks for 5 sec, is interruptable
    println("latch timed out")

    42  // Completed
  }

  f.onFailure { case ex => println(ex.getClass) }
  f.onSuccess { case i => println(i) }

  Thread.sleep(6000)   // Set to less than 5000 to cancel

  val wasCancelled = cancel()

  println("wasCancelled: " + wasCancelled)

    Thread.sleep(2000) */

  //ref
  try {
    withTimeout(3 * 1000) {
      //withTimeout2(5 second) {
      test
    }
  } catch {
    case tde: ThreadDeath ⇒ println("DEATH2")
    case _ @e             ⇒ println("SOME EXCEPTION2" + e) //; throw new ThreadDeath
  }

  /*  try {
    Await.result(Future { test }, 5 seconds)
  } catch {
    case tde: ThreadDeath => println("DEATH2")
    case _                => println("SOME EXCEPTION2"); throw new ThreadDeath
  }*/
  //while (true) {}
}

class TimeOutException extends Exception

object withTimeout {
  def apply[T](to: Long)(f: ⇒ T): T = {
    var result: Either[Throwable, T] = Left(new TimeOutException())

    val r = new Runnable {
      def run(): Unit = {
        try {
          result = Right(f)
        } catch {
          case e: Exception ⇒
            result = Left(e)
        }
      }
    }

    val t = new Thread(r)
    t.start()
    t.join(to)
    if (t.isAlive()) {
      //t.interrupt()
      t.stop()
    }

    if (result.isLeft) throw result.left.get
    else result.right.get
  }
}

object withTimeout2 {
  def apply[T](to: Duration)(f: ⇒ T): T = {
    Await.result(Future { f }, to)
  }
}

/*object withTimeoutORD {
  def apply[T](to: Duration)(f: => T): T = {
    val (f, cancel) = TmpLabs.interruptableFutureV( f => {
  while(!future.isCompleted) continueCalculation // isCompleted acts as our interrupted-flag

  12  // when we're done, return some result
})

}*/
