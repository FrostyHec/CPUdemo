package example

import chisel3._
import chiseltest._
import org.scalatest._
import org.scalatest.matchers.should.Matchers
import example.day2.DFF
class DFFTester extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "DFF"

  it should "correctly store input value" in {
    test(new DFF) { dut =>
      dut.io.d.poke(false.B) //设置为false
      dut.clock.step(1) //推进一个时钟周期
      dut.io.q.expect(false.B) //查看是否真的是false

      dut.io.d.poke(true.B)
      dut.clock.step(1)
      dut.io.q.expect(true.B)

      dut.io.d.poke(false.B)
      dut.clock.step(1)
      dut.io.q.expect(false.B)
    }
  }
}