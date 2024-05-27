package device

import chisel3._
import chisel3.util.HasBlackBoxResource

class MMIOVGABundle extends Bundle {
  val value = Input(UInt(32.W))
}

class BoardVGABundle extends Bundle {
  val hsync = Output(Bool())
  val vsync = Output(Bool())
  val vga_rgb = Output(UInt(12.W))
}

class VGA extends Module {
  val io = IO(new Bundle() {
    val board = new BoardVGABundle
    val mmio = new MMIOVGABundle
  })
  val vga = Module(new VGABlackBox())
  vga.io.digit := io.mmio.value
  io.board.hsync := vga.io.hsync
  io.board.vsync := vga.io.vsync
  io.board.vga_rgb := vga.io.vga_rgb
}
class VGABlackBox extends BlackBox with HasBlackBoxResource {
  val io = IO(new Bundle() {
    val digit = Input(UInt(32.W))
    val hsync = Output(Bool())
    val vsync = Output(Bool())
    val vga_rgb = Output(UInt(12.W))
  })

  override def desiredName: String = "vga"

  addResource("/verilog/vga.v")
}