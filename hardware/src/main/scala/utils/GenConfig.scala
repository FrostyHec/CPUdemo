package utils

import chisel3._


case class GenConfig(
                      debugMode: Boolean,
                      useIPMemory: Boolean,
                      addressWidth: Int = 32, //这两个都是不可调节的
                      dataWidth: Int = 32, //这两个都是不可调节的
                      insMemSize: Int=1<<16, //64KB
                      dataMemSize: Int=1<<16, //64KB
                      insBegin: UInt="h00000000".U,
                      insEnd:UInt="h0000ffff".U,
                      dataBegin: UInt="h00010000".U,
                      dataEnd:UInt="h0001ffff".U,
                      _MMIOConfig: MMIOConfig=MMIOConfig.selected
                         )
object GenConfig {
  def selected = onBoard
  private def onBoard = GenConfig(
    debugMode = false,
    useIPMemory = false
  )
}