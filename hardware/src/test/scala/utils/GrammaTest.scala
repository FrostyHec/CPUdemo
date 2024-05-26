package utils

import chisel3._
import chisel3.util._
import chisel3.util.random._

class CacheRecord(val addrWidth: Int, val dataWidth: Int) extends Bundle {
  val valid = Bool()
  val address = UInt(addrWidth.W)
  val instruction = UInt(addrWidth.W)
  val data = UInt(dataWidth.W)
}

class Cache(val cacheSize: Int, val addrWidth: Int,
            val instructionWidth: Int, val dataWidth: Int) extends Module {
  val io = IO(new Bundle {
    val addr = Input(UInt(addrWidth.W))
    val instruction = Input(UInt(instructionWidth.W))
    val dataIn = Input(UInt(dataWidth.W))
    val writeEnable = Input(Bool())

    val dataOut = Output(UInt(dataWidth.W)) //output the prediction
  })

  // Create an array of cache lines
  val cache = RegInit(VecInit(Seq.fill(cacheSize)(0.U.asTypeOf(new CacheRecord(addrWidth, dataWidth)))))
  val random_gen = Module(new PRNGWrapper(log2Down(cacheSize)))
  // Default output values
  io.dataOut := 0.U

  // Write logic
  when(io.writeEnable) {
    val write_idx = Wire(UInt(log2Up(cacheSize).W))
    write_idx := random_gen.io.random // random replace if full
    (cacheSize - 1 to 0 by -1).foreach { i =>
      when(!cache(i).valid || cache(i).address === io.addr) {
        write_idx := i.U
      }
    }
    cache(write_idx).valid := true.B
    cache(write_idx).address := io.addr
    cache(write_idx).data := io.dataIn
  }
  // Read logic
  for (i <- 0 until cacheSize) {
    when(cache(i).valid && cache(i).address === io.addr) {
      io.dataOut := cache(i).data
    }
  }
}

class PRNGWrapper(width: Int) extends Module {
  val io = IO(new Bundle {
    val random = Output(UInt(width.W))
  })
  if (width < 2) {
    io.random := 0.U //random failed
  } else {
    // 实例化 FibonacciLFSR
    val lfsr = Module(new MaxPeriodFibonacciLFSR(width))

    // 将 LFSR 的输出连接到 IO
    io.random := lfsr.io.out

    // 你可以根据需要设置种子和其他参数
    lfsr.io.seed := 1.U // 设置初始种子（如果需要）
    lfsr.io.increment := true.B // 使能 LFSR 递增
  }
}

object Cache extends App {
  println(
    new(chisel3.stage.ChiselStage).emitVerilog(
      new Cache(2, 32, 32,32),
      Array(
        "--target-dir", "generated_dut/"
      )
    )
  )
}

