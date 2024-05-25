package utils

import Generate.Top
import chisel3._
import chiseltest._
import configs.GenConfig
import core.CoreTop
import core.config.CSRDebugIO

object InsUtils {
  private val file_root_path = "src/test/scala/integrate/inst_file/"
  private val INS_TIME = 3 // cycles for ins to run

  def load_instructions(file_path: String): Unit = {
    GenConfig.s.initInsFile = Option(file_root_path + file_path)
  }

  def run_instructions(cpu: Module, times: Int = 1): Unit = {
    for (i <- 1 to times) {
      println("============================Time " + i + "============================\n")
      cpu.clock.step(INS_TIME)
    }
    cpu.clock.step(4*INS_TIME)
  }

  def checkRegsInCPU(cpu: CoreTop, targetReg: Int, targetValue: UInt): Unit = {
    require(GenConfig.s.debugMode, "Debug mode is not enabled.")

    cpu.debug_io match {
      case Some(debugIO) =>
        //        printf(debugIO.reg_vals.reg_vals.peek().litValue.toString())
        debugIO.reg_vals.reg_vals(targetReg).expect(targetValue)
      case None =>
        println("Debug IO is not available. Ensure that debug mode is enabled.")
    }
  }

  def getCSRInCPU(cpu: CoreTop): CSRDebugIO = {
    require(GenConfig.s.debugMode, "Debug mode is not enabled.")
    cpu.debug_io match {
      case Some(debugIO) =>
        debugIO.csr_vals
    }
  }

  def checkRegsInTop(cpu: Top, targetReg: Int, targetValue: UInt): Unit = {
    require(GenConfig.s.debugMode, "Debug mode is not enabled.")

    cpu.debug_io match {
      case Some(debugIO) =>
        //        printf(debugIO.reg_vals.reg_vals.peek().litValue.toString())
        debugIO.reg_vals.reg_vals(targetReg).expect(targetValue)
      case None =>
        println("Debug IO is not available. Ensure that debug mode is enabled.")
    }
  }
}