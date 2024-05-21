package device

import chisel3._
import configs.GenConfig
import utils._

class MMIOUARTBundle extends Bundle {
  val rxData = Output(UInt(8.W)) // 接收数据
  val rxValid = Output(Bool()) // 接收数据有效信号

  val txData = Input(UInt(8.W)) // 发送数据
  val txStart = Input(Bool()) // 发送开始信号
  val txReady = Output(Bool()) // 发送器准备好信号
}

class BoardUARTBundle extends Bundle {
  val rx = Input(Bool()) // 串行输入信号
  val tx = Output(Bool()) // 串行输出信号
}

// 整体UART模
//TODO 整体模块上层的rst必须要取反成为low_enable
class UART extends Module {
  val io = IO(new Bundle {
    val board = new BoardUARTBundle
    val mmio = new MMIOUARTBundle
  })

  val uartRx = Module(new UARTrx)
  val uartTx = Module(new UARTtx)

  uartRx.io.rx := io.board.rx
  io.board.tx := uartTx.io.tx

  io.mmio.rxData := uartRx.io.data
  io.mmio.rxValid := uartRx.io.valid

  uartTx.io.data := io.mmio.txData
  uartTx.io.start := io.mmio.txStart
  io.mmio.txReady := uartTx.io.ready
}
