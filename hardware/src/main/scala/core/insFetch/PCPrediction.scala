package core.insFetch

import chisel3._
import chisel3.util._
import configs.GenConfig
import core.config._

class PredictionFailureBundle extends Bundle {
  val failure_occurs = Input(Bool())
  val success_occurs = Input(Bool())
  val address = Input(UInt(32.W))

  def setDefault(): Unit = {
    failure_occurs := false.B
    success_occurs := false.B
    address := 0.U
  }

  def setFailed(pc: UInt): Unit = {
    failure_occurs := true.B
    success_occurs := false.B
    address := pc
  }

  def setSuccess(pc: UInt): Unit = {
    failure_occurs := false.B
    success_occurs := true.B
    address := pc
  }
}

class PCPrediction extends Module {
  val io = IO(new Bundle() {
    val cpu_state_type = Input(CPUStateType.getWidth)
    val pc = Input(UInt(32.W))
    val instruction = Input(UInt(32.W))

    val prediction_failure = new PredictionFailureBundle()

    val predict_new_pc = Output(UInt(32.W))
  })
  io.predict_new_pc := io.pc + 4.U
  //jal special judge
  when(io.instruction(6, 0) === "b110_1111".U) {
    val raw_imm = Cat(Fill(11, io.instruction(31)),
      io.instruction(31, 31), io.instruction(19, 12),
      io.instruction(20, 20), io.instruction(30, 21))
    io.predict_new_pc := io.pc + (raw_imm << 1)
  }
  //TODO predict table
  //参考 https://www.cnblogs.com/arthurzyc/p/16895277.html
//   table: valid, pc, ins , previous (B type prediction)
  if (!GenConfig.s.cache_prediction_enable) {
    when(io.instruction(6, 0) === "b1100011".U) {
      val imm = Cat(Fill(20, io.instruction(31, 31)),
        io.instruction(31, 31), io.instruction(7, 7),
        io.instruction(30, 25), io.instruction(11, 8))
      io.predict_new_pc := io.pc + (imm << 1)
    }
  } else { //using cache
    val max_strong_no_jump = (math.pow(2, GenConfig.s.prediction_n) - 1).toInt
    val min_weak_jump = (math.pow(2, GenConfig.s.prediction_n - 1) - 1).toInt //0-1 jump, 2-3, no jump
    val cache = Module(new PredictionCache(
      GenConfig.s.prediction_cache_size,
      GenConfig.s.addressWidth,
      32,
      GenConfig.s.prediction_n,
      min_weak_jump.U
    ))
    cache.io.cpu_state_type:= io.cpu_state_type
    cache.io.addr_read := io.pc
    cache.io.read_en := false.B

    cache.io.addr_write := io.prediction_failure.address
    cache.io.dataIn := cache.io.write_data_out
    val prediction_result_get = io.prediction_failure.failure_occurs || io.prediction_failure.success_occurs
    cache.io.write_en := prediction_result_get

    //prediction module
    when(io.instruction(6, 0) === "b1100011".U) {
      val imm = Cat(Fill(20, io.instruction(31, 31)),
        io.instruction(31, 31), io.instruction(7, 7),
        io.instruction(30, 25), io.instruction(11, 8))
      cache.io.read_en := true.B
      when(cache.io.dataOut <= min_weak_jump.U) { //jumping
        io.predict_new_pc := io.pc + (imm << 1)
      }.otherwise {
        io.predict_new_pc := io.pc + 4.U
      }
    }
    //updating module
    when(prediction_result_get) {
      when(io.prediction_failure.failure_occurs) {
        when(cache.io.write_data_out <= min_weak_jump.U) { //predict jump but failed
          cache.io.dataIn := cache.io.write_data_out + 1.U
        }.otherwise {
          cache.io.dataIn := cache.io.write_data_out - 1.U
        }
      }.otherwise { //success
//        printf("cur wdout: %d\n",cache.io.write_data_out)
        when(cache.io.write_data_out <= min_weak_jump.U) { //predict jump and success
          when(cache.io.write_data_out > 0.U) {
            cache.io.dataIn := cache.io.write_data_out - 1.U
          }
        }.otherwise {
          when(cache.io.write_data_out < max_strong_no_jump.U) {
            cache.io.dataIn := cache.io.write_data_out + 1.U
          }
        }
      }
    }
  }
}
