package core.insFetch

import chisel3._
/*PC register
*
*/
class PC extends Module{
  val io=Module(new Bundle() {
    val nextAddr =Input(UInt(32.W))
    val addr=Output(UInt(32.W))
  })
  io.addr:=io.nextAddr
}
