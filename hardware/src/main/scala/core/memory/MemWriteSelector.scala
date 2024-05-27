package core.memory
import chisel3._
import core.config._
class MemWriteSelector extends Module {
  //就是个selector
  val io = IO(new Bundle() {
    val cpu_state = Input(CPUStateType.getWidth)
    //uart
    val uart_in = Flipped(new UARTMemBundle)

    //mem (建议改改)
    val cpu_read_data = Input(Bool())
    val cpu_write_data = Input(Bool())

    val cpu_data_width = Input(DataWidth.getWidth)

    val cpu_data_addr = Input(UInt(32.W))
    val cpu_data_write = Input(UInt(32.W))

    //out
    val read_data = Output(Bool())
    val write_data = Output(Bool())

    val data_width = Output(DataWidth.getWidth)

    val data_addr = Output(UInt(32.W))
    val data_write = Output(UInt(32.W))
  })
  //TODO 这个When语句造成combinational loop
  when(io.cpu_state===CPUStateType.sLoadMode.getUInt){
    io.read_data:=io.uart_in.mem_read
    io.write_data:=io.uart_in.mem_write
    io.data_width:=io.uart_in.data_width
    io.data_addr:=io.uart_in.data_addr
    io.data_write:=io.uart_in.data_to_write
  }.otherwise{
    io.read_data:=io.cpu_read_data
    io.write_data:=io.cpu_write_data
    io.data_width:=io.cpu_data_width
    io.data_addr:=io.cpu_data_addr
    io.data_write:=io.cpu_data_write
  }
}
