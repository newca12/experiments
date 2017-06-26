package computation

import org.scalatest._
import org.scalatest.Matchers
import org.scalatest.concurrent.TimeLimits
import org.scalatest.time.{Millis, Span}

class interruptSpec extends WordSpec with Matchers with TimeLimits {

  "computation" should {

    "finish in due time" in {
      failAfter(Span(100, Millis)) {
        Thread.sleep(50)
      }
    }
  }

}
