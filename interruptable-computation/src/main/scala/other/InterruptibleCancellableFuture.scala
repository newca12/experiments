package other

import java.util.concurrent.CancellationException
import scala.concurrent.Await
//import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.Promise
import scala.concurrent.duration.Duration
import scala.concurrent.duration.DurationInt
import java.util.concurrent.TimeoutException

object withTimeoutOld {
  def apply[T](to: Duration)(f: ⇒ T): T = {
    Await.result(Future { f }, to)
  }
}

object withTimeout {
  def interruptableFuture[T](fun: Future[T] ⇒ T) /*(implicit ex: ExecutionContext)*/: (Future[T], () ⇒ Boolean) = {
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
      lock.synchronized { updateCurrentThread(thread) }
      try fun(f)
      finally {
        val wasInterrupted = lock.synchronized { updateCurrentThread(null) } ne thread
        //Deal with interrupted flag of this thread in desired
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

  def apply[T](a: T)(timeout: Duration)(fun: Future[T] ⇒ T): T = {
    val (f, cancel) = interruptableFuture[T](future ⇒ {
      fun(future)
    })
    var res: T = a
    try {
      res = Await.result(f, timeout)
    } catch {
      case ie: InterruptedException ⇒ println("INTERRUPTED")
      case e: TimeoutException      ⇒ println("TIMEOUT")
    }

    val wasCancelled = cancel()
    res
  }
}

object interruptableFuture extends App {

  import scala.sys.process.Process
  import scala.sys.process.ProcessLogger
  def run(commandLine: String, time: Long, timeOut: Duration = 50 seconds): String = {
    println("running ...")
    val div        = 1.0E6 //microseconde
    var p: Process = null
    var out        = List[String]()
    var err        = List[String]()
    //TODO quick hack but mutable list can be avoid
    val t = scala.collection.mutable.MutableList(System.nanoTime)
    //TODO use loan pattern (see https://wiki.scala-lang.org/display/SYGN/Loan)
    //http://stackoverflow.com/questions/12362994/scala-how-to-avoid-mutables-inside-try-catch-blocks
    //http://alvinalexander.com/scala/scala-exception-allcatch-examples-option-try-either
    try {
      p = Process("/Users/hack/EDLA/C/signalINT", None, "TMPDIR" -> "/tmp")
        .run(ProcessLogger((s) ⇒ out ::= s, (s) ⇒ err ::= s))
      t += System.nanoTime
      forkProcess(p, timeOut)
      t += System.nanoTime
    } catch {
      case e: Throwable ⇒
        println(e + "detected => DESTROY")
        if (p != null) p.destroy()
        throw e
    }
    val exit = p.exitValue
    if (exit != 0) println(s"ERR ${err.reverse}")
    val res = if (exit != 0) err.reverse else out
    if ((exit == 0)) {
      val ext   = err.head
      val total = (t(2) - time) / div
      println(
        f"${(t(0) - time) / div}%.0f ${(t(1) - time) / div}%.0f ${total - ext.toDouble * 1000}%.0f ${total}%.0f CMD (${commandLine})")
    }
    res.toString
  }

  import java.util.concurrent.Executors
  //import java.util.concurrent.Future
  import java.util.concurrent.TimeUnit
  import java.util.concurrent.Callable
  private def forkProcess(proc: Process, timeout: Duration): Unit = {
    val executor = Executors.newSingleThreadExecutor()
    val future: java.util.concurrent.Future[Unit] = executor.submit(new Callable[Unit] {
      def call: Unit = { proc.exitValue() }
    })
    try {
      future.get(timeout.toSeconds, TimeUnit.SECONDS)
    } catch {
      case to: TimeoutException ⇒
        future.cancel(true)
        throw to
    } finally {
      executor.shutdown()
    }
  }

  def interruptableFuture[T](fun: Future[T] ⇒ T) /*(implicit ex: ExecutionContext)*/: (Future[T], () ⇒ Boolean) = {
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
      lock.synchronized { updateCurrentThread(thread) }
      try fun(f)
      finally {
        val wasInterrupted = lock.synchronized { updateCurrentThread(null) } ne thread
        //Deal with interrupted flag of this thread in desired
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

  def longComputation(): Int = {
    var i = 0
    try {
      // isCompleted acts as our interrupted-flag
      while (i < 10) { print(i); Thread.sleep(300); i = i + 1 }
    } catch {
      case ie: InterruptedException ⇒ println("INTERRUPTED1")
      case e: TimeoutException      ⇒ println("TIMEOUT1")
    }
    i
  }

  def externalComputation(): Int = {
    try {
      var res = run("...", System.nanoTime)
      println(res)
    } catch {
      case e: Throwable ⇒ println("run Exception:" + e)
    }
    println("async ?")
    1
  }

  def runner[T](timeout: Duration, fun: ⇒ T) = {
    //val timeout = 2 second
    val executor = Executors.newSingleThreadExecutor()
    val future: java.util.concurrent.Future[Unit] = executor.submit(new Callable[Unit] {
      def call: Unit = { fun }
    })
    try {
      future.get(timeout.toSeconds, TimeUnit.SECONDS)
    } catch {
      case to: TimeoutException ⇒
        println("cancel future")
        future.cancel(true)
        throw to
    } finally {
      executor.shutdown()
    }
  }

  try {
    runner(3 second, longComputation _)
  } catch {
    case e: Throwable ⇒ println("end runner:" + e)
  }

  Thread.sleep(10000)

  /*  try {
    val res = withTimeout[Int](0)(2 second) {
      //try {
        val t = externalComputation _ //longComputation _
        t
      //} catch {
      //  case ie: InterruptedException => println("INTERRUPTED")
      //  case e: TimeoutException      => println("TIMEOUT")
        //case e: Throwable => println("SOME EXCEPTION" + e.getMessage)
      //}
    }
    println("res:"+res)
  } catch {
    case ie: InterruptedException => println("INTERRUPTED2")
    case e: TimeoutException      => println("TIMEOUT2")
    //case e: Throwable => println("SOME EXCEPTION" + e.getMessage)
  }*/

}
