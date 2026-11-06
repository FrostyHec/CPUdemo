package core.writeBack

import chiseltest._
import org.scalatest._
import org.scalatest.matchers.should.Matchers
class AUSelectorTest extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "AUSelector"

  it should "correctly do sth" in {
    test(new AUSelector) { AUSelector =>

    }
  }
}
