package utils

import Generate.Top
import chisel3._
import chiseltest._
import configs.GenConfig
import device.UART

object UARTUtils {
  val baud_count = GenConfig.s.board.uart_baud_count //10000000/115200

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
    require(data.getWidth <= 8)
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


  //shit code
  def initFromTop(top: Top): Unit = {
    top.io.uart.rx.poke(true.B)
  }

  def sendSingleRxFromTop(top: Top, bit: Bool): Unit = {
    top.io.uart.rx.poke(bit)
    for (_ <- 0 until baud_count) {
      top.clock.step()
    }
  }

  def sendRxFromTop(top: Top, data: UInt): Unit = {
    require(data.getWidth <= 8)
    sendSingleRxFromTop(top, false.B) //start
    for (i <- 0 until 8) { //8 data
      sendSingleRxFromTop(top, data(i))
      println(s"sent data ${i} val: ${data(i).litToBoolean}")
    }
    sendSingleRxFromTop(top, true.B) //stop
  }

  def sendWordFromTop(top: Top, data: UInt): Unit = {
    require(data.getWidth <= 32)
    for (i <- 0 until 4) { //8 data
      sendRxFromTop(top, data(i * 8 + 7, i * 8))
      println(s"sent BYTE ${i} val: ${data(i * 8 + 7, i * 8).litValue}")
      top.clock.step(10)//todo?
    }
  }
}
