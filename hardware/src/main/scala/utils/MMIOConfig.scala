package utils

import chisel3._


case class MMIOConfig(begin: UInt = "hffffff00".U,
                      end: UInt = "hffffffff".U
                     )

object MMIOConfig {
  def selected: MMIOConfig = default

  private def default = MMIOConfig()
}
