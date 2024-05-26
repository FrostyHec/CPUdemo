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
                      insMemIPCoreName: Option[String] = Some("ins_mem"),
                      dataMemIPCoreName: Option[String] = Some("data_mem"),
                      ramAddressWidth:Int = 14,

                      // for debug use
                      debugMode: Boolean, // wire connecting reg to top
                      logDetails: Boolean = false,
                      var initInsFile: Option[String] = None,

                      //for pipeline prediction
                      logPrediction:Boolean = false,
                      cache_prediction_enable:Boolean = true,
                      prediction_cache_size:Int = 16,
                      prediction_n:Int = 2,

                      //clock
                      useIPClock: Boolean = false,
                      ipClockName: Option[String] = Some("clk_wiz_0"),

                      useIPUART: Boolean = false
                    ) {
}

object GenConfig {
//  def s = forTest
//  def s = onBoard
  def s = predictionCount

  private val onBoard = GenConfig(
    debugMode = false,
    useIPMemory = true,
    useIPUART = true,
    useIPClock = true,
  )

  private val forTest = GenConfig(
    debugMode = true,
    useIPMemory = false,
    logDetails = true
  )
  private val predictionCount =GenConfig(
    debugMode = true,
    useIPMemory = false,
    logPrediction = true
  )
}