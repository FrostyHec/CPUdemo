package utils

import chisel3._

class RWMemoryPort(addrWidth: Int, dataWidth: Int) extends Bundle {
  val write = Input(Bool())

  val read_addr = Input(UInt(addrWidth.W))

  val write_addr = Input(UInt(addrWidth.W))
  val write_data = Input(UInt(dataWidth.W))

  val read_data = Output(UInt(dataWidth.W))

  def assign(write: Data, read_addr: Data, write_addr: Data, write_data: Data, read_data: Data): Unit = {
    this.write := write
    this.read_addr := read_addr
    this.write_addr := write_addr
    this.write_data := write_data
    if(read_data!=DontCare){
      read_data:=this.read_data
    }
  }
}

class RAM(addrWidth: Int, datWidth: Int, size: Int, ipConfig: Option[String] = None) extends Module { //size in byte
  val io = IO(new RWMemoryPort(addrWidth, datWidth))
  if (ipConfig.isDefined) {
    println("Unimplemented Instruction Memory using IP") //TODO IP core
  }
  else {
    val mem = Mem(size, UInt(datWidth.W))
    when(io.write) {
      mem.write(io.write_addr, io.write_data)
    }
    io.read_data := mem.read(io.read_addr)
  }
}
