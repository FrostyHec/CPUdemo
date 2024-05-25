package core.decode
import chisel3._
import chisel3.util._
import core.config.ForwardType
//rs1,rs2,csr前面都有着一个selector控制真实数据的输出
class ForwardSelector extends Module {
  val io =IO(new Bundle() {
    val forward_type = Input(ForwardType.getWidth)

    val origin_val = Input(UInt(32.W))
    val ex_forward = Input(UInt(32.W))
    val mem_forward = Input(UInt(32.W))

    val real_val = Output(UInt(32.W))
  })
  io.real_val := io.origin_val//default value
  switch(io.forward_type) {
    is(ForwardType.EXForward.getUInt) {
      io.real_val := io.ex_forward
    }
    is(ForwardType.MEMForward.getUInt) {
      io.real_val := io.mem_forward
    }
    is(ForwardType.No.getUInt) {
      io.real_val := io.origin_val
    }
  }
}
