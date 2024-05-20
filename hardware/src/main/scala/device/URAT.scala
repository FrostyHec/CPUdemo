package device

import chisel3._
import configs.GenConfig

class URATMMIOBundle extends Bundle {
  val switches = Output(UInt(GenConfig.s._MMIO.switchWidth.W))
}

class URATBoardBundle extends Bundle {
  val switches = Input(UInt(GenConfig.s.board.switchWidth.W))
}

//TODO 注意：提供的URAT 的rst是high_enable的
//TODO 整体模块上层的rst必须要取反成为low_enable
class URATWrapper extends Module {
  
}
