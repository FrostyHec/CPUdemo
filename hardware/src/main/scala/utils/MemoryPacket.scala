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
    if (read_data != DontCare) {
      read_data := this.read_data
    }
  }
}

class RAM(addrWidth: Int, datWidth: Int, size: Int, ipConfig: Option[String] = None, initFile: Option[String] = None) extends Module { //size in byte
  val io = IO(new RWMemoryPort(addrWidth, datWidth))

  if (ipConfig.isDefined) {
    println("Unimplemented Instruction Memory using IP") //TODO IP core
  }
  else {
    val mem = Mem(size, UInt(datWidth.W))
    initFile.foreach(file => {
      println("load from " + file + "\n")
      chisel3.util.experimental.loadMemoryFromFile(mem, file)
    })

    when(io.write) {
      mem.write(io.write_addr, io.write_data)
    }
    io.read_data := mem.read(io.read_addr)
  }
}

//dual port ram https://blog.csdn.net/apple_53311083/article/details/132239286 一口读写一口异步读，对于同时写的，是下一刻才读
class RMemoryPort(addrWidth: Int, dataWidth: Int) extends Bundle {
  val read_addr = Input(UInt(addrWidth.W))
  val read_data = Output(UInt(dataWidth.W))
}

class DualPortRAM(addrWidth: Int, datWidth: Int, size: Int, ipConfig: Option[String] = None, initFile: Option[String] = None) extends Module { //size in byte
  val io = IO(new RWMemoryPort(addrWidth, datWidth))
  val io2 = IO(new RMemoryPort(addrWidth, datWidth))

  if (ipConfig.isDefined) {
    println("Unimplemented Instruction Memory using IP") //TODO IP core
  }
  else {
    val mem = Mem(size, UInt(datWidth.W))
    initFile.foreach(file => {
      println("load from " + file + "\n")
      chisel3.util.experimental.loadMemoryFromFile(mem, file)
    })

    when(io.write) {
      mem.write(io.write_addr, io.write_data)
    }
    io.read_data := mem.read(io.read_addr)
    io2.read_data := mem.read(io2.read_addr)
  }
}