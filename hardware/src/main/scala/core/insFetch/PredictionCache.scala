package core.insFetch

import chisel3.util._
import chisel3._
import configs.GenConfig
import utils.PRNGWrapper

class CacheRecord(val addrWidth: Int, val dataWidth: Int) extends Bundle {
  val valid = Bool()
  val address = UInt(addrWidth.W)
  val instruction = UInt(addrWidth.W)
  val data = UInt(dataWidth.W)
}

class Cache(val cacheSize: Int, val addrWidth: Int,
            val instructionWidth: Int, val dataWidth: Int, val default_data: UInt) extends Module {
  val io = IO(new Bundle {
    val addr = Input(UInt(addrWidth.W))
    val instruction = Input(UInt(instructionWidth.W))
    val dataIn = Input(UInt(dataWidth.W))
    val writeEnable = Input(Bool())

    val dataOut = Output(UInt(dataWidth.W)) //output the prediction
  })

  // Create an array of cache lines
  val cache = RegInit(VecInit(Seq.fill(cacheSize)(0.U.asTypeOf(new CacheRecord(addrWidth, dataWidth)))))
  val random_gen = Module(new PRNGWrapper(log2Down(cacheSize))) // random replacement

  io.dataOut := default_data // Default output values

  // Write logic
  when(io.writeEnable) {
    val write_idx = Wire(UInt(log2Up(cacheSize).W))
    write_idx := random_gen.io.random


    val cache_hit = Wire(Bool())
    (cacheSize - 1 to 0 by -1).foreach { i =>
      when(cache(i).valid && cache(i).address === io.addr && cache(i).instruction===io.instruction) {
        //setting record as new value
        write_idx := i.U
        cache_hit:=true.B
      }
    }
    when(!cache_hit){
      (cacheSize - 1 to 0 by -1).foreach { i =>
        //otherwise using a new place to store record
        when(!cache(i).valid) {
          write_idx := i.U
        }
      }
    }

    cache(write_idx).valid := true.B
    cache(write_idx).address := io.addr
    cache(write_idx).data := io.dataIn
  }
  // read logic
  for (i <- 0 until cacheSize) {
    when(cache(i).address === io.addr) {
      when(cache(i).instruction =/= io.instruction) {
        if (GenConfig.s.logDetails) {
          printf("Instruction replacement, resulting invalid")
        }
        //setting invalid when finding addr-ins not same
        cache(i).valid := false.B
      }.otherwise {
        io.dataOut := cache(i).data
      }
    }
  }
}
