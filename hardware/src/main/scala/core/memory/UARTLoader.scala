package core.memory

import chisel3._
import chisel3.util._
import core.config._

class UARTMemBundle extends Bundle {
  val mem_read = Output(Bool()) //always false
  val mem_write = Output(Bool())

  val data_to_write = Output(UInt(32.W))

  val data_addr = Output(UInt(32.W))

  val data_width = Output(DataWidth.getWidth) //always bytes
}

class UARTLoader extends Module {
  val io = IO(new Bundle() {
    //state
    val cpu_state = Input(CPUStateType.getWidth)

    //input from uart
    val rxValid = Input(Bool())

    val rxData = Input(UInt(8.W))

    //output to mem
    val mem = new UARTMemBundle
  })
  val idle :: start :: write_data :: wait_data :: Nil = Enum(4)

  val cur_state = RegInit(idle)
  val data_addr = RegInit("hffff_ffff".U(32.W))

  //always, 状态机控制mem_write
  io.mem.data_addr := data_addr
  io.mem.data_width := DataWidth.Byte.getUInt
  io.mem.data_to_write := io.rxData
  io.mem.mem_read := false.B
  io.mem.mem_write := false.B // default

  switch(cur_state) {
    is(idle) {
      when(io.cpu_state === CPUStateType.sLoadMode.getUInt) {
        data_addr := "hffff_ffff".U(32.W)
        cur_state := start
      }
    }
    is(start) {
      when(io.cpu_state =/= CPUStateType.sLoadMode.getUInt) {
        cur_state := idle
      }.otherwise {
        when(io.rxValid === false.B) {
          cur_state := wait_data
        }
      }
    }
    is(wait_data) {
      when(io.cpu_state =/= CPUStateType.sLoadMode.getUInt) {
        cur_state := idle
      }.otherwise {
        when(io.rxValid === true.B) {
          data_addr := data_addr + 1.U
          cur_state := write_data
        }
      }
    }
    is(write_data) {
      when(io.cpu_state =/= CPUStateType.sLoadMode.getUInt) {
        cur_state := idle
      }.otherwise {
        when(io.rxValid === true.B) {
          io.mem.mem_write := true.B
        }.otherwise {
          cur_state := wait_data
        }
      }
    }
  }
}
