package core.utils

import chisel3._
import chiseltest._
import configs.GenConfig
import device.UARTWrapper
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.UARTUtils._


class UARTWrapperTest extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "UART"

  it should "receive data correctly" in {
    test(new UARTWrapper) { dut =>
      for (dt <- 0 to 255) {
        for (wt <- 1 to baud_count) {
          init(dut)
          dut.clock.step(wt)

          val data = dt.U(8.W)
          sendRx(dut, data)
          dut.clock.step(10)
          dut.io.mmio.rxValid.expect(true.B)
          dut.io.mmio.rxData.expect(data)
        }
      }
    }
  }

  it should "transmit data correctly" in {
    test(new UARTWrapper) { dut =>
      for (dt <- 0 to 255) {
        println(s"cur val::$dt")
        for (wt <- 1 to baud_count) {
          init(dut)
          dut.clock.step(wt)
          val data = dt.U(8.W)
          //          val data = 1.U(8.W)
          sendData(dut, data)
          checkTx(dut, data)
          //          dut.clock.step(2)
          dut.io.mmio.txReady.expect(true.B)
        }
      }
    }
  }
}