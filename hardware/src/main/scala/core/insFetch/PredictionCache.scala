package core.insFetch

import chisel3.util._
import chisel3._
import configs.GenConfig
import utils.PRNGWrapper

class PredictionCacheRecord(val addrWidth: Int, val dataWidth: Int) extends Bundle {
  val valid = Bool()
  val address = UInt(addrWidth.W)
  val data = UInt(dataWidth.W)
}

class PredictionCache(val cacheSize: Int, val addrWidth: Int,
                      val instructionWidth: Int, val dataWidth: Int, val default_data: UInt) extends Module {
  val io = IO(new Bundle {
    val addr_read = Input(UInt(addrWidth.W))
    val read_en = Input(Bool())

    val addr_write = Input(UInt(addrWidth.W))
    val dataIn = Input(UInt(dataWidth.W))
    val write_en = Input(Bool())
    val write_data_out = Output(UInt(dataWidth.W))

    val dataOut = Output(UInt(dataWidth.W)) //output the prediction
  })

  // cache array
  val cache = RegInit(VecInit(Seq.fill(cacheSize)(0.U.asTypeOf(new PredictionCacheRecord(addrWidth, dataWidth)))))
  val random_gen = Module(new PRNGWrapper(log2Down(cacheSize))) // random replacement

  io.dataOut := default_data // Default output values

  // Write logic
  when(io.write_en) {
    val write_idx = Wire(UInt(log2Up(cacheSize).W))
    write_idx := random_gen.io.random

    val cache_hit = Wire(Bool())
    (cacheSize - 1 to 0 by -1).foreach { i =>
      when(cache(i).address === io.addr_write) {
        //setting record as new value
        write_idx := i.U
        cache_hit := true.B
      }
    }
    when(!cache_hit) {
      (cacheSize - 1 to 0 by -1).foreach { i =>
        //otherwise using a new place to store record
        when(!cache(i).valid) {
          write_idx := i.U
        }
      }
    }
    io.write_data_out:=Mux(cache_hit,cache(write_idx),default_data)
    cache(write_idx).valid := true.B
    cache(write_idx).address := io.addr_write
    cache(write_idx).data := io.dataIn
  }
  // read logic
  when(io.read_en) {
    val cache_hit_read = Wire(Bool())
    cache_hit_read := false.B
    for (i <- 0 until cacheSize) {
      when(cache(i).valid && cache(i).address === io.addr_write) {
        io.dataOut := cache(i).data
        cache_hit_read := true.B
      }
    }
    when(!cache_hit_read) {
      //finding a new place and store the record as default
      val write_idx_read = Wire(UInt(log2Up(cacheSize).W))
      write_idx_read := random_gen.io.random
      (cacheSize - 1 to 0 by -1).foreach { i =>
        //otherwise using a new place to store record
        when(!cache(i).valid) {
          write_idx_read := i.U
        }
      }
      cache(write_idx_read).valid := true.B
      cache(write_idx_read).address := io.addr_read
      cache(write_idx_read).data := default_data
    }
  }
}
