package core.dataAccess

import chiseltest._
import core.dataAccess.MemoryDispatch
import org.scalatest._
import org.scalatest.matchers.should.Matchers
class MemoryDispatchTest extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "ALU"

  it should "correctly do sth" in {
    test(new MemoryDispatch) {memoryDispatch =>

    }
  }
}