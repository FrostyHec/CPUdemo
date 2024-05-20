package device

import chisel3._
import configs.GenConfig

class MMIOLedBundle extends Bundle {
  val led = Input(UInt(GenConfig.s._MMIO.ledWidth.W))
}

class BoardLedBundle extends Bundle {
  val led = Output(UInt(GenConfig.s.board.ledWidth.W))
}

class Led extends Module {
  val io = IO(new Bundle {
    val mmio = new MMIOLedBundle
    val board = new BoardLedBundle
  })

  io.board.led := io.mmio.led

//  // 0.5s 切换高低16位
//  val clockFrequency = 50000000
//  val oneSecondCount = clockFrequency.U
//
//  val counter = RegInit(0.U(32.W))
//
//  val stateReg = RegInit(false.B) // false:低16位，true:高16位
//
//  counter := counter + 1.U
//
//  // 0.5s 切换
//  when(counter === oneSecondCount) {
//    counter := 0.U
//    stateReg := !stateReg
//  }
//
//  io.board.led := Mux(stateReg, io.mmio.led(31, 16), io.mmio.led(15, 0))

}
