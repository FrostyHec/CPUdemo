//package utils
//
//import chisel3._
//import chisel3.util._
//
//class UART(val DATA_WIDTH: Int) extends Module {
//  val io = IO(new Bundle {
////    val clk = Input(Clock())
////    val rst = Input(Bool())
//
//    // AXI input
//    val s_axis_tdata = Input(UInt(DATA_WIDTH.W))
//    val s_axis_tvalid = Input(Bool())
//    val s_axis_tready = Output(Bool())
//
//    // AXI output
//    val m_axis_tdata = Output(UInt(DATA_WIDTH.W))
//    val m_axis_tvalid = Output(Bool())
//    val m_axis_tready = Input(Bool())
//
//    // UART interface
//    val rxd = Input(Bool())
//    val txd = Output(Bool())
//
//    // Status
//    val tx_busy = Output(Bool())
//    val rx_busy = Output(Bool())
//    val rx_overrun_error = Output(Bool())
//    val rx_frame_error = Output(Bool())
//
//    // Configuration
//    val prescale = Input(UInt(16.W))
//  })
//
//  val uartTx = Module(new UARTTx(DATA_WIDTH))
//  val uartRx = Module(new UARTRx(DATA_WIDTH))
//
////  uartTx.io.clk := io.clk
////  uartTx.io.rst := io.rst
//  uartTx.io.s_axis_tdata := io.s_axis_tdata
//  uartTx.io.s_axis_tvalid := io.s_axis_tvalid
//  io.s_axis_tready := uartTx.io.s_axis_tready
//  uartTx.io.prescale := io.prescale
//  io.txd := uartTx.io.txd
//  io.tx_busy := uartTx.io.busy
//
////  uartRx.io.clk := io.clk
////  uartRx.io.rst := io.rst
//  uartRx.io.m_axis_tready := io.m_axis_tready
//  uartRx.io.rxd := io.rxd
//  uartRx.io.prescale := io.prescale
//  io.m_axis_tdata := uartRx.io.m_axis_tdata
//  io.m_axis_tvalid := uartRx.io.m_axis_tvalid
//  io.rx_busy := uartRx.io.busy
//  io.rx_overrun_error := uartRx.io.overrun_error
//  io.rx_frame_error := uartRx.io.frame_error
//}
//
//class UARTTx(val DATA_WIDTH: Int) extends Module {
//  val io = IO(new Bundle {
////    val clk = Input(Clock())
////    val rst = Input(Bool())
//
//    // AXI input
//    val s_axis_tdata = Input(UInt(DATA_WIDTH.W))
//    val s_axis_tvalid = Input(Bool())
//    val s_axis_tready = Output(Bool())
//
//    // UART interface
//    val txd = Output(Bool())
//
//    // Status
//    val busy = Output(Bool())
//
//    // Configuration
//    val prescale = Input(UInt(16.W))
//  })
//
//  val s_axis_tready_reg = RegInit(false.B)
//  val txd_reg = RegInit(true.B)
//  val busy_reg = RegInit(false.B)
//  val data_reg = RegInit(0.U((DATA_WIDTH + 1).W))
//  val prescale_reg = RegInit(0.U(19.W))
//  val bit_cnt = RegInit(0.U(4.W))
//
//  io.s_axis_tready := s_axis_tready_reg
//  io.txd := txd_reg
//  io.busy := busy_reg
//
//  when(reset.asBool) {
//    s_axis_tready_reg := false.B
//    txd_reg := true.B
//    prescale_reg := 0.U
//    bit_cnt := 0.U
//    busy_reg := false.B
//  }.otherwise {
//    when(prescale_reg > 0.U) {
//      s_axis_tready_reg := false.B
//      prescale_reg := prescale_reg - 1.U
//    }.elsewhen(bit_cnt === 0.U) {
//      s_axis_tready_reg := true.B
//      busy_reg := false.B
//
//      when(io.s_axis_tvalid) {
//        s_axis_tready_reg := !s_axis_tready_reg
//        prescale_reg := (io.prescale << 3).asUInt - 1.U
//        bit_cnt := (DATA_WIDTH + 1).U
//        data_reg := Cat(1.U(1.W), io.s_axis_tdata)
//        txd_reg := false.B
//        busy_reg := true.B
//      }
//    }.otherwise {
//      when(bit_cnt > 1.U) {
//        bit_cnt := bit_cnt - 1.U
//        prescale_reg := (io.prescale << 3).asUInt - 1.U
//        data_reg := data_reg >> 1
//        txd_reg := data_reg(0)
//      }.elsewhen(bit_cnt === 1.U) {
//        bit_cnt := bit_cnt - 1.U
//        prescale_reg := io.prescale << 3
//        txd_reg := true.B
//      }
//    }
//  }
//}
//
//class UARTRx(val DATA_WIDTH: Int) extends Module {
//  val io = IO(new Bundle {
////    val clk = Input(Clock())
////    val rst = Input(Bool())
//
//    // AXI output
//    val m_axis_tdata = Output(UInt(DATA_WIDTH.W))
//    val m_axis_tvalid = Output(Bool())
//    val m_axis_tready = Input(Bool())
//
//    // UART interface
//    val rxd = Input(Bool())
//
//    // Status
//    val busy = Output(Bool())
//    val overrun_error = Output(Bool())
//    val frame_error = Output(Bool())
//
//    // Configuration
//    val prescale = Input(UInt(16.W))
//  })
//
//  val m_axis_tdata_reg = RegInit(0.U(DATA_WIDTH.W))
//  val m_axis_tvalid_reg = RegInit(false.B)
//  val rxd_reg = RegNext(io.rxd, true.B)
//  val busy_reg = RegInit(false.B)
//  val overrun_error_reg = RegInit(false.B)
//  val frame_error_reg = RegInit(false.B)
//  val data_reg = RegInit(0.U(DATA_WIDTH.W))
//  val prescale_reg = RegInit(0.U(19.W))
//  val bit_cnt = RegInit(0.U(4.W))
//
//  io.m_axis_tdata := m_axis_tdata_reg
//  io.m_axis_tvalid := m_axis_tvalid_reg
//  io.busy := busy_reg
//  io.overrun_error := overrun_error_reg
//  io.frame_error := frame_error_reg
//
//  when(reset.asBool) {
//    m_axis_tdata_reg := 0.U
//    m_axis_tvalid_reg := false.B
//    prescale_reg := 0.U
//    bit_cnt := 0.U
//    busy_reg := false.B
//    overrun_error_reg := false.B
//    frame_error_reg := false.B
//  }.otherwise {
//    rxd_reg := io.rxd
//    overrun_error_reg := false.B
//    frame_error_reg := false.B
//
//    when(io.m_axis_tvalid && io.m_axis_tready) {
//      m_axis_tvalid_reg := false.B
//    }
//
//    when(prescale_reg > 0.U) {
//      prescale_reg := prescale_reg - 1.U
//    }.elsewhen(bit_cnt > 0.U) {
//      when(bit_cnt > (DATA_WIDTH + 1).U) {
//        when(!rxd_reg) {
//          bit_cnt := bit_cnt - 1.U
//          prescale_reg := (io.prescale << 3).asUInt - 1.U
//        }.otherwise {
//          bit_cnt := 0.U
//          prescale_reg := 0.U
//        }
//      }.elsewhen(bit_cnt > 1.U) {
//        bit_cnt := bit_cnt - 1.U
//        prescale_reg := (io.prescale << 3).asUInt - 1.U
//        data_reg := Cat(rxd_reg, data_reg(DATA_WIDTH - 1, 1))
//      }.elsewhen(bit_cnt === 1.U) {
//        bit_cnt := bit_cnt - 1.U
//        when(rxd_reg) {
//          m_axis_tdata_reg := data_reg
//          m_axis_tvalid_reg := true.B
//          overrun_error_reg := m_axis_tvalid_reg
//        }.otherwise {
//          frame_error_reg := true.B
//        }
//      }
//    }.otherwise {
//      busy_reg := false.B
//      when(!rxd_reg) {
//        prescale_reg := (io.prescale << 2).asUInt - 2.U
//        bit_cnt := (DATA_WIDTH + 2).U
//        data_reg := 0.U
//        busy_reg := true.B
//      }
//    }
//  }
//}
//
//package utils
//
//import chisel3._
//import chisel3.util._
//
//class UART(val DATA_WIDTH: Int = 8) extends Module {
//  val io = IO(new Bundle {
//    // AXI input
//    val s_axis_tdata = Input(UInt(DATA_WIDTH.W))
//    val s_axis_tvalid = Input(Bool())
//    val s_axis_tready = Output(Bool())
//
//    // AXI output
//    val m_axis_tdata = Output(UInt(DATA_WIDTH.W))
//    val m_axis_tvalid = Output(Bool())
//    val m_axis_tready = Input(Bool())
//
//    // UART interface
//    val rxd = Input(Bool())
//    val txd = Output(Bool())
//
//    // Status
//    val tx_busy = Output(Bool())
//    val rx_busy = Output(Bool())
//    val rx_overrun_error = Output(Bool())
//    val rx_frame_error = Output(Bool())
//
//    // Configuration
//    val prescale = Input(UInt(16.W))
//  })
//
//  val uartTx = Module(new UARTTx(DATA_WIDTH))
//  val uartRx = Module(new UARTRx(DATA_WIDTH))
//
//  uartTx.io.s_axis_tdata := io.s_axis_tdata
//  uartTx.io.s_axis_tvalid := io.s_axis_tvalid
//  io.s_axis_tready := uartTx.io.s_axis_tready
//  uartTx.io.prescale := io.prescale
//  io.txd := uartTx.io.txd
//  io.tx_busy := uartTx.io.busy
//
//  uartRx.io.m_axis_tready := io.m_axis_tready
//  uartRx.io.rxd := io.rxd
//  uartRx.io.prescale := io.prescale
//  io.m_axis_tdata := uartRx.io.m_axis_tdata
//  io.m_axis_tvalid := uartRx.io.m_axis_tvalid
//  io.rx_busy := uartRx.io.busy
//  io.rx_overrun_error := uartRx.io.overrun_error
//  io.rx_frame_error := uartRx.io.frame_error
//}
//
//class UARTTx(val DATA_WIDTH: Int = 8) extends Module {
//  val io = IO(new Bundle {
//    // AXI input
//    val s_axis_tdata = Input(UInt(DATA_WIDTH.W))
//    val s_axis_tvalid = Input(Bool())
//    val s_axis_tready = Output(Bool())
//
//    // UART interface
//    val txd = Output(Bool())
//
//    // Status
//    val busy = Output(Bool())
//
//    // Configuration
//    val prescale = Input(UInt(16.W))
//  })
//
//  val s_axis_tready_reg = RegInit(false.B)
//  val txd_reg = RegInit(true.B)
//  val busy_reg = RegInit(false.B)
//  val data_reg = RegInit(0.U((DATA_WIDTH + 1).W))
//  val prescale_reg = RegInit(0.U(19.W))
//  val bit_cnt = RegInit(0.U(4.W))
//
//  io.s_axis_tready := s_axis_tready_reg
//  io.txd := txd_reg
//  io.busy := busy_reg
//
//  when(reset.asBool) {
//    s_axis_tready_reg := false.B
//    txd_reg := true.B
//    prescale_reg := 0.U
//    bit_cnt := 0.U
//    busy_reg := false.B
//  }.otherwise {
//    when(prescale_reg > 0.U) {
//      s_axis_tready_reg := false.B
//      prescale_reg := prescale_reg - 1.U
//    }.elsewhen(bit_cnt === 0.U) {
//      s_axis_tready_reg := true.B
//      busy_reg := false.B
//
//      when(io.s_axis_tvalid) {
//        s_axis_tready_reg := !s_axis_tready_reg
//        prescale_reg := (io.prescale << 3).asUInt - 1.U
//        bit_cnt := (DATA_WIDTH + 1).U
//        data_reg := Cat(1.U(1.W), io.s_axis_tdata)
//        txd_reg := false.B
//        busy_reg := true.B
//      }
//    }.otherwise {
//      when(bit_cnt > 1.U) {
//        bit_cnt := bit_cnt - 1.U
//        prescale_reg := (io.prescale << 3).asUInt - 1.U
//        data_reg := data_reg >> 1
//        txd_reg := data_reg(0)
//      }.elsewhen(bit_cnt === 1.U) {
//        bit_cnt := bit_cnt - 1.U
//        prescale_reg := io.prescale << 3
//        txd_reg := true.B
//      }
//    }
//  }
//}
//
//class UARTRx(val DATA_WIDTH: Int = 8) extends Module {
//  val io = IO(new Bundle {
//    // AXI output
//    val m_axis_tdata = Output(UInt(DATA_WIDTH.W))
//    val m_axis_tvalid = Output(Bool())
//    val m_axis_tready = Input(Bool())
//
//    // UART interface
//    val rxd = Input(Bool())
//
//    // Status
//    val busy = Output(Bool())
//    val overrun_error = Output(Bool())
//    val frame_error = Output(Bool())
//
//    // Configuration
//    val prescale = Input(UInt(16.W))
//  })
//
//  val m_axis_tdata_reg = RegInit(0.U(DATA_WIDTH.W))
//  val m_axis_tvalid_reg = RegInit(false.B)
//  val rxd_reg = RegNext(io.rxd, true.B)
//  val busy_reg = RegInit(false.B)
//  val overrun_error_reg = RegInit(false.B)
//  val frame_error_reg = RegInit(false.B)
//  val data_reg = RegInit(0.U(DATA_WIDTH.W))
//  val prescale_reg = RegInit(0.U(19.W))
//  val bit_cnt = RegInit(0.U(4.W))
//
//  io.m_axis_tdata := m_axis_tdata_reg
//  io.m_axis_tvalid := m_axis_tvalid_reg
//  io.busy := busy_reg
//  io.overrun_error := overrun_error_reg
//  io.frame_error := frame_error_reg
//
//  when(reset.asBool) {
//    m_axis_tdata_reg := 0.U
//    m_axis_tvalid_reg := false.B
//    prescale_reg := 0.U
//    bit_cnt := 0.U
//    busy_reg := false.B
//    overrun_error_reg := false.B
//    frame_error_reg := false.B
//  }.otherwise {
//    rxd_reg := io.rxd
//    overrun_error_reg := false.B
//    frame_error_reg := false.B
//
//    when(io.m_axis_tvalid && io.m_axis_tready) {
//      m_axis_tvalid_reg := false.B
//    }
//
//    when(prescale_reg > 0.U) {
//      prescale_reg := prescale_reg - 1.U
//    }.elsewhen(bit_cnt > 0.U) {
//      when(bit_cnt > (DATA_WIDTH + 1).U) {
//        when(!rxd_reg) {
//          bit_cnt := bit_cnt - 1.U
//          prescale_reg := (io.prescale << 3).asUInt - 1.U
//        }.otherwise {
//          bit_cnt := 0.U
//          prescale_reg := 0.U
//        }
//      }.elsewhen(bit_cnt > 1.U) {
//        bit_cnt := bit_cnt - 1.U
//        prescale_reg := (io.prescale << 3).asUInt - 1.U
//        data_reg := Cat(rxd_reg, data_reg(DATA_WIDTH - 1, 1))
//      }.elsewhen(bit_cnt === 1.U) {
//        bit_cnt := bit_cnt - 1.U
//        when(rxd_reg) {
//          m_axis_tdata_reg := data_reg
//          m_axis_tvalid_reg := true.B
//          overrun_error_reg := m_axis_tvalid_reg
//        }.otherwise {
//          frame_error_reg := true.B
//        }
//      }
//    }.otherwise {
//      busy_reg := false.B
//      when(!rxd_reg) {
//        prescale_reg := (io.prescale << 2).asUInt - 2.U
//        bit_cnt := (DATA_WIDTH + 2).U
//        data_reg := 0.U
//        busy_reg := true.B
//      }
//    }
//  }
//}
//class UARTRx(val DATA_WIDTH: Int = 8) extends Module {
//  val io = IO(new Bundle {
//    // AXI output
//    val m_axis_tdata = Output(UInt(DATA_WIDTH.W))
//    val m_axis_tvalid = Output(Bool())
//    val m_axis_tready = Input(Bool())
//
//    // UART interface
//    val rxd = Input(Bool())
//
//    // Status
//    val busy = Output(Bool())
//    val overrun_error = Output(Bool())
//    val frame_error = Output(Bool())
//
//    // Configuration
//    val prescale = Input(UInt(16.W))
//  })
//
//  val m_axis_tdata_reg = RegInit(0.U(DATA_WIDTH.W))
//  val m_axis_tvalid_reg = RegInit(false.B)
//  val rxd_reg = RegNext(io.rxd, true.B)
//  val busy_reg = RegInit(false.B)
//  val overrun_error_reg = RegInit(false.B)
//  val frame_error_reg = RegInit(false.B)
//  val data_reg = RegInit(0.U(DATA_WIDTH.W))
//  val prescale_reg = RegInit(0.U(19.W))
//  val bit_cnt = RegInit(0.U(4.W))
//
//  io.m_axis_tdata := m_axis_tdata_reg
//  io.m_axis_tvalid := m_axis_tvalid_reg
//  io.busy := busy_reg
//  io.overrun_error := overrun_error_reg
//  io.frame_error := frame_error_reg
//
//  when(reset.asBool) {
//    m_axis_tdata_reg := 0.U
//    m_axis_tvalid_reg := false.B
//    prescale_reg := 0.U
//    bit_cnt := 0.U
//    busy_reg := false.B
//    overrun_error_reg := false.B
//    frame_error_reg := false.B
//  }.otherwise {
//    rxd_reg := io.rxd
//    overrun_error_reg := false.B
//    frame_error_reg := false.B
//
//    when(io.m_axis_tvalid && io.m_axis_tready) {
//      m_axis_tvalid_reg := false.B
//    }
//
//    when(prescale_reg > 0.U) {
//      prescale_reg := prescale_reg - 1.U
//    }.elsewhen(bit_cnt > 0.U) {
//      when(bit_cnt > (DATA_WIDTH + 1).U) {
//        when(!rxd_reg) {
//          bit_cnt := bit_cnt - 1.U
//          prescale_reg := (io.prescale << 3).asUInt - 1.U
//        }.otherwise {
//          bit_cnt := 0.U
//          prescale_reg := 0.U
//        }
//      }.elsewhen(bit_cnt > 1.U) {
//        bit_cnt := bit_cnt - 1.U
//        prescale_reg := (io.prescale << 3).asUInt - 1.U
//        data_reg := Cat(rxd_reg, data_reg(DATA_WIDTH - 1, 1))
//      }.elsewhen(bit_cnt === 1.U) {
//        bit_cnt := bit_cnt - 1.U
//        when(rxd_reg) {
//          m_axis_tdata_reg := data_reg
//          m_axis_tvalid_reg := true.B
//          overrun_error_reg := m_axis_tvalid_reg
//        }.otherwise {
//          frame_error_reg := true.B
//        }
//      }
//    }.otherwise {
//      busy_reg := false.B
//      when(!rxd_reg) {
//        prescale_reg := (io.prescale << 2).asUInt - 2.U
//        bit_cnt := (DATA_WIDTH + 2).U
//        data_reg := 0.U
//        busy_reg := true.B
//      }
//    }
//  }
//}
package utils

import chisel3._
import chisel3.util._
import configs.GenConfig

object DebugDetailConfig{
  val test_rx = true
  val test_tx = false
}

// UART接收器模块
class UARTrx extends Module {
  val io = IO(new Bundle {
    val rx = Input(Bool()) // 串行输入信号

    val data = Output(UInt(8.W)) // 并行输出数据
    val valid = Output(Bool()) // 数据有效信号
  })
  val baud_cout = GenConfig.s.board.uart_baud_count.U - 1.U

  val idle :: start :: recv :: stop :: Nil = Enum(4)
  val state = RegInit(idle)
  val bitCount = RegInit(0.U(4.W))
  val data_queues = RegInit(0.U(8.W))

  //TODO 调整波特率
  // 假设波特率计数器已经在其他地方定义
  val baudCounter = RegInit(0.U(16.W))
  val baudTick = (baudCounter === 0.U)

  val io_valid = RegInit(false.B)
  val io_data = RegInit(0.U(8.W))

  io.data := io_data
  io.valid := io_valid

  when(baudTick) { //TODO change kick
    if (GenConfig.s.logDetails && DebugDetailConfig.test_rx) {
      printf("======Baud Tick Occurs=====\n")
    }
    baudCounter := baud_cout
  }.otherwise {
    baudCounter := baudCounter - 1.U
  }

  switch(state) {
    is(idle) {
      when(!io.rx) {
        io_valid := false.B
        io_data := 0.U
        state := start
        baudCounter := baud_cout - 1.U
      }
    }
    is(start) {
      when(baudTick) {
        if (GenConfig.s.logDetails) {
          printf("---RX will goto RECV\n")
        }
        state := recv
        baudCounter := baud_cout
        bitCount := 0.U
      }
    }
    is(recv) {
      when(baudTick) {
        if (GenConfig.s.logDetails) {
          printf(s"RX prev data: %b\n", data_queues)
          printf("Will set bits %d\n", bitCount)
        }
        data_queues := Cat(io.rx, data_queues >> 1)
        bitCount := bitCount + 1.U
        when(bitCount === 7.U) {
          state := stop
        }
      }
    }
    is(stop) {
      when(baudTick) {
        if (GenConfig.s.logDetails) {
          printf(s"RX STOP data: %b\n", data_queues)
        }
        state := idle
        io_valid := true.B
        io_data := data_queues
      }
    }
  }
  if (GenConfig.s.logDetails && DebugDetailConfig.test_rx) {
    printf("baud tick %d , value: %d ", baudTick, baudCounter)
    printf("CURRENT RX: %b, cur que %b ", io.rx, data_queues)
    printf("RX cur valid: %b , cur data: %b\n", io.valid, io.data)
  }
}

// UART发送器模块
class UARTtx extends Module {
  val io = IO(new Bundle {
    val tx = Output(Bool()) // 串行输出信号
    val data = Input(UInt(8.W)) // 并行输入数据
    val start = Input(Bool()) // 发送开始信号
    val ready = Output(Bool()) // 发送器准备好信号
  })
  val baud_cout = GenConfig.s.board.uart_baud_count.U - 1.U

  val idle :: start :: data :: stop :: Nil = Enum(4)
  val state = RegInit(idle)
  val bitCount = RegInit(0.U(4.W))
  val shiftReg = RegInit(0.U(8.W))

  val baudCounter = RegInit(0.U(16.W))
  val baudTick = (baudCounter === 0.U)


  io.ready := (state === idle)

  val io_tx = RegInit(true.B)

  io.tx := io_tx

  when(baudTick) { //TODO change kick
    baudCounter := baud_cout
  }.otherwise {
    baudCounter := baudCounter - 1.U
  }

  switch(state) {
    is(idle) {
      when(io.start) {
        state := start
        shiftReg := io.data
        baudCounter := baud_cout
      }
    }
    is(start) {
      when(baudTick) {
        if (GenConfig.s.logDetails) {
          printf("---TX will goto SEND\n")
        }
        state := data
        baudCounter := 0.U
        bitCount := 0.U
      }
      io_tx := false.B
    }
    is(data) {
      when(baudTick) {
        if (GenConfig.s.logDetails) {
          printf(s"TX prev data : %b, bit cnt %d\n", shiftReg,bitCount)
        }
        io_tx := shiftReg(0)
        shiftReg := shiftReg >> 1
        bitCount := bitCount + 1.U
        when(bitCount === 8.U) {
          state := stop
          io_tx:=true.B
        }
      }
    }
    is(stop) {
      io_tx := true.B
      when(baudTick) {
        if (GenConfig.s.logDetails) {
          printf(s"TX STOP data: %b\n", shiftReg)
        }
        state := idle
      }
    }
  }
  if (GenConfig.s.logDetails && DebugDetailConfig.test_tx) {
    printf("baud tick %d , value: %d ", baudTick, baudCounter)
    printf("CURRENT TX: %b, cur que %b ", io.tx, shiftReg)
    printf("TX cur ready: %b \n", io.ready)
  }
}
//package utils
//import chisel3._
//import chisel3.util._
//
//class UART_rx(
//               bitRate: Int = 115200,
//               clkFreq: Int = 16000000,
//               payloadBits: Int = 8
//             ) extends Module {
//  val io = IO(new Bundle() {
//    val i_serial_data = Input(Bool())
//    val o_rx_done = Output(Bool())
//    val o_data = Output(UInt(payloadBits.W))
//  })
//
//  val clksPerBit = clkFreq / bitRate // clocks per bit (CPB)
//  val clkCnterBW = log2Ceil(clksPerBit) + 1 // bit width for clkCnterReg
//  val bitCnterBW = log2Ceil(payloadBits) + 1 // bit width for bitCnterReg
//
//  // fsm states
//  val idle :: startBit :: dataBits :: stopBit :: Nil = Enum(4)
//
//  val clkCnterReg = RegInit(0.U(clkCnterBW.W)) // counting clk edges
//  val bitCnterReg = RegInit(0.U(bitCnterBW.W)) // counting bits transmitted
//
//  val outDataReg = RegInit(VecInit(Seq.fill(payloadBits)(false.B))) // output data reg
//  val outRxDoneReg = RegInit(false.B) // output rx done reg
//  val stateReg = RegInit(idle) // fsm state reg
//
//  // input serial data synchronized to clk domain
//  val serialDataReg = RegNext(RegNext(io.i_serial_data))
//
//  io.o_data := Cat(outDataReg.reverse)
//  io.o_rx_done := outRxDoneReg
//
//  switch(stateReg) {
//    is(idle) {
//      outRxDoneReg := false.B
//      // counters disabled
//      clkCnterReg := 0.U(clkCnterBW.W)
//      bitCnterReg := 0.U(bitCnterBW.W)
//      // if serial input data low, go to startBit
//      when(serialDataReg === false.B) {
//        stateReg := startBit
//      }.otherwise {
//        stateReg := idle
//      }
//    }
//    is(startBit) {
//      when(clkCnterReg < (clksPerBit / 2).U) {
//        clkCnterReg := clkCnterReg + 1.U
//        stateReg := startBit
//      }.otherwise {
//        // we are at the middle of start bit
//        clkCnterReg := 0.U(clkCnterBW.W)
//        when(serialDataReg === false.B) {
//          stateReg := dataBits
//        }.otherwise {
//          // treat as noise and go back to idle
//          stateReg := idle
//        }
//      }
//    }
//    is(dataBits) {
//      when(clkCnterReg < clksPerBit.U) {
//        clkCnterReg := clkCnterReg + 1.U
//      }.otherwise {
//        clkCnterReg := 0.U(clkCnterBW.W)
//        // we are at the middle of a data bit
//        // grab data bit and update bitCnterReg
//        outDataReg(bitCnterReg) := serialDataReg
//        when(bitCnterReg < payloadBits.U) {
//          bitCnterReg := bitCnterReg + 1.U
//        }.otherwise {
//          bitCnterReg := 0.U(bitCnterBW.W)
//        }
//      }
//      when(bitCnterReg === payloadBits.U) {
//        stateReg := stopBit
//      }.otherwise {
//        stateReg := dataBits
//      }
//    }
//    is(stopBit) {
//      when(clkCnterReg < clksPerBit.U) {
//        clkCnterReg := clkCnterReg + 1.U
//        stateReg := stopBit
//      }.otherwise {
//        clkCnterReg := 0.U(clkCnterBW.W)
//        // we are at the middle of stop bit
//        outRxDoneReg := true.B
//        stateReg := idle
//      }
//    }
//  }
//}
//
//class UART_tx(
//               bitRate: Int = 115200,
//               clkFreq: Int = 10000000,
//               payloadBits: Int = 8
//             ) extends Module {
//  val io = IO(new Bundle() {
//    val i_tx_trig = Input(Bool())
//    val i_data = Input(UInt(payloadBits.W))
//    val o_tx_busy = Output(Bool())
//    val o_tx_done = Output(Bool())
//    val o_serial_data = Output(Bool())
//  })
//
//  val clksPerBit = clkFreq / bitRate // clocks per bit (CPB)
//  val clkCnterBW = log2Ceil(clksPerBit) + 1 // bit width for clkCnterReg
//  val bitCnterBW = log2Ceil(payloadBits) + 1 // bit width for bitCnterReg
//
//  // fsm states
//  val idle :: startBit :: dataBits :: stopBit :: Nil = Enum(4)
//
//  val clkCnterReg = RegInit(0.U(clkCnterBW.W)) // counting clk edges
//  val bitCnterReg = RegInit(0.U(bitCnterBW.W)) // counting bits transmitted
//
//  val inDataReg = RegInit(0.U(payloadBits.W)) // input data reg
//  val outDataReg = RegInit(true.B) // output data reg
//  val outTxBusyReg = RegInit(false.B) // output tx busy reg
//  val outTxDoneReg = RegInit(false.B) // output tx done reg
//  val stateReg = RegInit(idle) // fsm state reg
//
//  io.o_serial_data := outDataReg
//  io.o_tx_busy := outTxBusyReg
//  io.o_tx_done := outTxDoneReg
//  outTxBusyReg := stateReg =/= idle
//
//  switch(stateReg) {
//    is(idle) {
//      outTxDoneReg := false.B
//      // counters disabled
//      clkCnterReg := 0.U(clkCnterBW.W)
//      bitCnterReg := 0.U(bitCnterBW.W)
//      // if trigger signal high, transmit start bit and reg input data, go to startBit
//      when(io.i_tx_trig === true.B) {
//        outDataReg := false.B
//        // register input data at the beginning of transmission
//        inDataReg := io.i_data
//        stateReg := startBit
//      }.otherwise {
//        stateReg := idle
//      }
//    }
//    is(startBit) {
//      when(clkCnterReg < clksPerBit.U) {
//        clkCnterReg := clkCnterReg + 1.U
//        stateReg := startBit
//      }.otherwise {
//        clkCnterReg := 0.U(clkCnterBW.W)
//        bitCnterReg := bitCnterReg + 1.U
//        // transmit first data bit
//        outDataReg := inDataReg(bitCnterReg)
//        stateReg := dataBits
//      }
//    }
//    is(dataBits) {
//      when(clkCnterReg < clksPerBit.U) {
//        clkCnterReg := clkCnterReg + 1.U
//        stateReg := dataBits
//      }.otherwise {
//        clkCnterReg := 0.U(clkCnterBW.W)
//        when(bitCnterReg < payloadBits.U) {
//          // transmit next data bit
//          outDataReg := inDataReg(bitCnterReg)
//          bitCnterReg := bitCnterReg + 1.U
//          stateReg := dataBits
//        }.otherwise {
//          bitCnterReg := 0.U(bitCnterBW.W)
//          // transmit stop bit
//          outDataReg := true.B
//          stateReg := stopBit
//        }
//      }
//    }
//    is(stopBit) {
//      when(clkCnterReg < clksPerBit.U) {
//        clkCnterReg := clkCnterReg + 1.U
//        stateReg := stopBit
//      }.otherwise {
//        clkCnterReg := 0.U(clkCnterBW.W)
//        outTxDoneReg := true.B
//        stateReg := idle
//      }
//    }
//  }
//}
//class UART extends Module {
//  val io = IO(new Bundle {
//    val rx = Input(Bool()) // 串行输入信号
//    val tx = Output(Bool()) // 串行输出信号
//    val rxData = Output(UInt(8.W)) // 接收数据
//    val rxValid = Output(Bool()) // 接收数据有效信号
//    val txData = Input(UInt(8.W)) // 发送数据
//    val txStart = Input(Bool()) // 发送开始信号
//    val txReady = Output(Bool()) // 发送器准备好信号
//  })
//
//  val uartRx = Module(new UART_rx)
//  val uartTx = Module(new UART_tx)
//
//  uartRx.io.i_serial_data := io.rx
//  io.rxData := uartRx.io.o_data
//  io.rxValid := uartRx.io.o_rx_done
//
//  uartTx.io.i_data := io.txData
//  uartTx.io.i_tx_trig := io.txStart
//  io.tx := uartTx.io.o_serial_data
//  io.txReady := uartTx.io.o_tx_busy
//}