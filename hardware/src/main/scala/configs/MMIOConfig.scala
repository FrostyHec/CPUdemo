package configs

import chisel3._

case class MMIOConfig(begin: UInt = "hffffff00".U,
                      end: UInt = "hffffffff".U,

                     //mmio parameters
                     //WARNING: IF WITH IS CHANGED, THE MAPPING LOGIC IN MMIO SHOULD ALSO BE CHANGED
                      ledWidth: Int = 32,
                      btnWidth: Int = 32,
                      switchWidth: Int = 32,
                      seg7Width: Int = 32,

                     //mmio address
                      ledAddr: UInt = "hffffff00".U,

                      btnAddr: UInt = "hffffff04".U,

                      switchAddr: UInt = "hffffff08".U,

                      seg7Addr: UInt = "hffffff0c".U

                     )

object MMIOConfig {
  def selected: MMIOConfig = default

  private def default = MMIOConfig()
}
