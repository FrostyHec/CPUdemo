package core.writeBack

import chiseltest._
import org.scalatest._
import org.scalatest.matchers.should.Matchers
class WriteDataSelectorTest extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "WriteDataSelector"

  it should "correctly do sth" in {
    test(new WriteDataSelector) { writeDataSelector =>

    }
  }
}
