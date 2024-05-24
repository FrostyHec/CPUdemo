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
                      uartRxDataWidth: Int = 32,
                      uartRxValidWidth: Int = 32,
                      uartRxReadyWidth: Int = 32,
                      uartTxDataWidth: Int = 32,
                      uartTxValidWidth: Int = 32,
                      uartTxReadyWidth: Int = 32,

                      //mmio address
                      ledAddr: UInt = "hffffff00".U,

                      btnAddr: UInt = "hffffff04".U,

                      switchAddr: UInt = "hffffff08".U,

                      seg7Addr: UInt = "hffffff0c".U,

                      //vga
                      vgaHexAddr:UInt = "hffff_ff10".U,
                      //uart
                      uartRxAddr: UInt = "hffff_ff14".U,
                      uartRxValidAddr: UInt = "hffff_ff18".U,
                      uartRxReadyAddr: UInt = "hffff_ff1c".U,
                      uartTxAddr:UInt = "hffff_ff20".U,
                      uartTxValid:UInt = "hffff_ff24".U,
                      uartTxStart:UInt = "hffff_ff28".U
                     ) {

}

object MMIOConfig {
  def selected: MMIOConfig = default

  private def default = MMIOConfig()
}
