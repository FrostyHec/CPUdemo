package example.day2

import Chisel._
import chisel3.PrintableHelper

//class B extends Module {
//  val io = IO(new Bundle {
//    val a = Input(Bool())
//    val b = Output(Bool())
//  })
//  io.b := io.a
//}
class DFF extends Module {
  val io = IO(new Bundle {
    val d = Input(Bool())
    val q = Output(Bool())
  })
//  val b=Module(new B())
//  b.io.a:=io.d
//  io.q:=b.io.b
  val qReg=RegInit(false.B)
  qReg := io.d
  io.q := qReg
  printf("d=%d q=%d\n",io.d,io.q) //注意，这里面整个都是在一个always里的
  printf(p"now reg: ${qReg}\n") //string里千万不要有中文字符！！！！！！！！！！
}
object DFF extends App {
  // These lines generate the Verilog output
  println(
    new (chisel3.stage.ChiselStage).emitVerilog(
      new DFF(),
      Array(
        "--target-dir", "generated_dut/"
      )
    )
  )
}