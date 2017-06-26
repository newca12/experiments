package common

object Computations extends App {

  def longComputation(isInterruptible: Boolean, n: Int) =
    Some((new PiIterator(isInterruptible)).take(n).size)

  def brokenComputation(): Option[Int] = Some("fail".toInt)

  def fakeComputation(isInterruptible: Boolean): Option[Int] = {
    println("Start computation :" + Thread.currentThread() + ":" + Thread.currentThread().getId)
    try {
      if (isInterruptible)
        Thread.sleep(6000) //simulate an interruptible computation
      else while (true) {} //simulate a non-interruptible computation
      None
    } catch {
      case tde: ThreadDeath ⇒ //only triggered by deprecated Thread.stop
        println("ThreadDeath catched.")
        None
      case ie: InterruptedException ⇒ //triggered only if computation is interruptible
        println("InterruptedException catched => Computation interrupted")
        None
      case _ @e: Throwable ⇒
        println(e.getMessage)
        None
    } finally {
      println("Computation terminated.") //never reached till the end of the computation if the computation is non-interruptible
    }
  }

  // Example of a long computation that can be instantiated interruptible or not
  // modified from http://rosettacode.org/wiki/Pi#Scala
  class PiIterator(isInterruptible: Boolean) extends Iterable[BigInt] {
    var r: BigInt       = 0
    var q, t, k: BigInt = 1
    var n, l: BigInt    = 3
    var nr, nn: BigInt  = 0

    def iterator: Iterator[BigInt] = new Iterator[BigInt] {
      def hasNext = true

      def next(): BigInt = {
        if (isInterruptible && Thread.interrupted()) {
          throw new InterruptedException();
        }
        while ((4 * q + r - t) >= (n * t)) {
          nr = (2 * q + r) * l
          nn = (q * (7 * k) + 2 + (r * l)) / (t * l)
          q = q * k
          t = t * l
          l = l + 2
          k = k + 1
          n = nn
          r = nr
        }
        val ret = n
        nr = 10 * (r - n * t)
        n = ((10 * (3 * q + r)) / t) - (10 * n)
        q = q * 10
        r = nr
        ret
      }
    }
  }

}
