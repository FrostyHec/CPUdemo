package core.utils

import chisel3._
import chiseltest._
import configs.GenConfig
import device.UART
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class UARTTest extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "UART"

  var baud_count = GenConfig.s.board.uart_baud_count //10000000/115200

  def init(dut: UART): Unit = {
    dut.io.board.rx.poke(true.B)
    dut.io.mmio.txStart.poke(false.B)
  }

  def sendSingleRx(dut: UART, bit: Bool): Unit = {
    for (_ <- 0 until baud_count) {
      dut.io.board.rx.poke(bit)
      dut.clock.step()
    }
  }

  def sendRx(dut: UART, data: UInt): Unit = {
    sendSingleRx(dut, false.B) //start
    for (i <- 0 until 8) { //8 data
      sendSingleRx(dut, data(i))
      println(s"sent data ${i} val: ${data(i).litToBoolean}")
    }
    sendSingleRx(dut, true.B) //stop
  }

  def sendData(dut: UART, data: UInt): Unit = {
    dut.io.mmio.txData.poke(data)
    dut.io.mmio.txStart.poke(true.B)
    dut.clock.step(2)
    dut.io.mmio.txStart.poke(false.B)
  }

  def checkSingleTx(dut: UART, bit: Bool): Unit = {
    for (_ <- 0 until baud_count) {
      dut.io.board.tx.expect(bit)
      dut.io.mmio.txReady.expect(false.B)
      dut.clock.step()
    }
  }

  def checkTx(dut: UART, data: UInt): Unit = {
    checkSingleTx(dut, false.B) //start
    for (i <- 0 until 8) { //8 data
      checkSingleTx(dut, data(i))
    }
    checkSingleTx(dut, true.B) //stop
  }

  it should "receive data correctly" in {
    test(new UART) { dut =>
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
    test(new UART) { dut =>
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