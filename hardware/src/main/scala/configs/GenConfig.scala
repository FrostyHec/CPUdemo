package configs

import chisel3._


case class GenConfig(

                      useIPMemory: Boolean,
                      addressWidth: Int = 32, //这两个都是不可调节的
                      dataWidth: Int = 32, //这两个都是不可调节的
                      insMemSize: Int = 1 << 16, //64KB
                      dataMemSize: Int = 1 << 16, //64KB
                      insBegin: UInt = "h00000000".U,
                      insEnd: UInt = "h0000ffff".U,
                      dataBegin: UInt = "h00010000".U,
                      dataEnd: UInt = "h0001ffff".U,
                      _MMIO: MMIOConfig = MMIOConfig.selected,
                      board: BoardConfig = BoardConfig.selected,

                      //ip core name
                      insMemIPCoreName: Option[String] = None,
                      dataMemIPCoreName: Option[String] = None,

                      // for debug use
                      debugMode: Boolean,
                      logDetails: Boolean = false,
                      var initInsFile: Option[String] = None
                    ) {
}

object GenConfig {
  def s = forTest

  private val onBoard = GenConfig(
    debugMode = false,
    useIPMemory = true
  )

  private val forTest = GenConfig(
    debugMode = true,
    useIPMemory = false,
    logDetails = true
  )
}