package device

import Chisel.MuxLookup
import Generate.Top
import chisel3._
import configs.GenConfig

class MMIOSeg7Bundle extends Bundle {
  val seg7 = Input(UInt(GenConfig.s._MMIO.seg7Width.W))
}

class BoardSeg7Bundle extends Bundle {
  //TODO seg7 out board bundle
  val seg7 = Output(UInt(GenConfig.s.board.seg7Width.W)) // 7段 + 1位启用信号
//  val seg7_low = Output(UInt(GenConfig.s.board.seg7Width.W)) // 7段 + 1位启用信号
  val an = Output(UInt(GenConfig.s.board.anWidth.W)) // 片选信号
}

class Seg7 extends Module {
  val io = IO(new Bundle {
    val mmio = new MMIOSeg7Bundle
    val board = new BoardSeg7Bundle
  })
  //TODO seg7 logic
  val clock_counter = RegInit(100000.U(32.W))

  clock_counter := clock_counter - 1.U

  val counter = RegInit(0.U(3.W))

  when(clock_counter === 0.U) {
    clock_counter := 100000.U
    counter := counter + 1.U
  }.otherwise {
    // do nothing
  }

  //  val digits_low = VecInit((0 until 4).map(i => io.mmio.seg7(4 * i + 3, 4 * i)))
  val digits = VecInit((0 until 8).map(i => io.mmio.seg7(4 * i + 3, 4 * i)))

  //  val seg7Module_low = Module(new HexToSeg7())
  //  seg7Module_low.io.hexDigit := digits_low(counter)
  //  io.board.seg7_low := seg7Module_low.io.seg7

  val seg7Module = Module(new HexToSeg7())
  seg7Module.io.hexDigit := digits(counter)
  io.board.seg7 := seg7Module.io.seg7

  io.board.an := MuxLookup(counter, "b1111_1111".U, Seq(
    "b000".U -> "b1111_1110".U,
    "b001".U -> "b1111_1101".U,
    "b010".U -> "b1111_1011".U,
    "b011".U -> "b1111_0111".U,
    "b100".U -> "b1110_1111".U,
    "b101".U -> "b1101_1111".U,
    "b110".U -> "b1011_1111".U,
    "b111".U -> "b0111_1111".U
  ))
}

class HexToSeg7 extends Module {
  def hexTo7Seg(hex: UInt): UInt = {
    val seg7 = Wire(UInt(8.W))
    seg7 := MuxLookup(hex, "b1111111".U,
      Array(
        "h0".U -> "b00000011".U, // 0
        "h1".U -> "b10011111".U, // 1
        "h2".U -> "b00100101".U, // 2
        "h3".U -> "b00001101".U, // 3
        "h4".U -> "b10011011".U, // 4
        "h5".U -> "b01001001".U, // 5
        "h6".U -> "b01000001".U, // 6
        "h7".U -> "b00011111".U, // 7
        "h8".U -> "b00000001".U, // 8
        "h9".U -> "b00001001".U, // 9
        "ha".U -> "b00010001".U, // A
        "hb".U -> "b11000001".U, // B
        "hc".U -> "b01100011".U, // C
        "hd".U -> "b10000101".U, // D
        "he".U -> "b01100001".U, // E
        "hf".U -> "b01110001".U  // F
      )
    )
    seg7 // 返回值
  }

  val io = IO(new Bundle {
    val hexDigit = Input(UInt(4.W))
    val seg7 = Output(UInt(8.W))
  })

  io.seg7 := hexTo7Seg(io.hexDigit)
}
