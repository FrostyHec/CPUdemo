package device

import chisel3._
import configs.GenConfig
import utils._
import chisel3.util._

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
  if (GenConfig.s.useIPUART) {
    val uart = Module(new UART_IP())
    uart.io.clk := clock.asBool
    uart.io.rst := reset.asBool

    uart.io.s_axis_tdata := io.mmio.txData
    uart.io.s_axis_tvalid := io.mmio.txStart
    io.mmio.txReady := uart.io.s_axis_tready

    uart.io.rxd := io.board.rx
    io.board.tx := uart.io.txd

    io.mmio.rxData := uart.io.m_axis_tdata
    io.mmio.rxValid := uart.io.m_axis_tvalid
    uart.io.m_axis_tready := false.B //TODO check correctness

    uart.io.prescale := 1302.U // 10000000/(9600*8)

  } else {
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
}

class UART_IP extends BlackBox with HasBlackBoxResource {
  val io = IO(new Bundle() {
    val clk = Input(Bool())
    val rst = Input(Bool())

    val s_axis_tdata = Input(UInt(8.W))
    val s_axis_tvalid = Input(Bool())
    val s_axis_tready = Output(Bool())

    val m_axis_tdata = Output(UInt(8.W))
    val m_axis_tvalid = Output(Bool())
    val m_axis_tready = Input(Bool())

    val rxd = Input(Bool())
    val txd = Output(Bool())

    val tx_busy = Output(Bool())
    val rx_busy = Output(Bool())
    val rx_overrun_error = Output(Bool())
    val rx_frame_error = Output(Bool())

    val prescale = Input(UInt(16.W))
  })

  override def desiredName: String = "uart"

  addResource("/verilog/UART.v")
}