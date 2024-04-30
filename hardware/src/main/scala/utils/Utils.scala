package utils

import chisel3._
object Utils {
  def emitVerilog(module: Module): Unit = {
    // These lines generate the Verilog output
    println(
      new(chisel3.stage.ChiselStage).emitVerilog(
        module,//use your module class
        Array(
          "--target-dir", "generated_dut/"
        )
      )
    )
  }
}
