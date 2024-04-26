package core.insFetch

import chiseltest._
import org.scalatest._
import org.scalatest.matchers.should.Matchers
class PCTest extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "PCTest"

  it should "correctly store at sWritePC state" in {
    test(new PC) { pc =>

    }
  }
  it should "not change value at other state" in {
    test(new PC){ pc=>

    }
  }
  it should "not change value at non posedge" in {
    test(new PC){ pc=>

    }
  }
}
