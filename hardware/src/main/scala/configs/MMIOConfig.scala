package configs

import chisel3._

case class MMIOConfig(begin: UInt = "hffffff00".U,
                      end: UInt = "hffffffff".U,

                     //mmio parameters
                      ledWidth: Int = 32,
                      btnWidth: Int = 32,
                      switchWidth: Int = 32,
                      seg7Width: Int = 32,
                     ) {

}

object MMIOConfig {
  def selected: MMIOConfig = default

  private def default = MMIOConfig()
}
