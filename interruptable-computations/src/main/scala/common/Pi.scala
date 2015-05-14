package common

//sample for a long computation that can be instantiated interruptible or not

// http://rosettacode.org/wiki/Pi#Scala
object Pi extends App {
  class PiIterator(isInterruptible: Boolean) extends Iterable[BigInt] {
    var r: BigInt = 0
    var q, t, k: BigInt = 1
    var n, l: BigInt = 3
    var nr, nn: BigInt = 0

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

  //val it = new PiIterator(false)
  //println((it head) + "." + (it take 10000 mkString))

  def longComputation(isInterruptible: Boolean, n: Int) = (new PiIterator(isInterruptible)).take(n).size

}