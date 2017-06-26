package labo.merge.labo

import com.twitter.util.Future

object TwitterFuture extends App {
  def test(): Int = {
    println("test:" + Thread.currentThread() + ":" + Thread.currentThread().getId)
    try {
      //while (true) {}
      Thread.sleep(40000)
    } catch {
      case tde: ThreadDeath         ⇒ println("DEATH1")
      case ie: InterruptedException ⇒ println("INTERRUPTED")
      case e: Throwable             ⇒ println("SOME EXCEPTION1" + e)
    }
    println("EXITING")
    0
  }

  /*  val future = Future { test() }

  Thread.sleep(2000)
  future.cancel() // in 5.x
  //future.raise(new FutureCancelledException) // in 6.x
  Thread.sleep(5000)
  future.onFailure { f => println(f) }*/

  try {
    val f = Future { test }
    println("Is this a Future ?")
    Thread.sleep(2000)
    f.cancel()
    //f.raise(new FutureCancelledException)
    f.onFailure { case ex ⇒ println(ex.getClass) }
    f.onSuccess { case i  ⇒ println(i) }
  } catch {
    case _ @e ⇒ println("SOME EXCEPTION2" + e)
  }

}
