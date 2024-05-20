package integrate

import chisel3._
import chiseltest._
import configs.GenConfig
import core.CoreTop
import utils.InsUtils._
import org.scalatest._
import org.scalatest.matchers.should.Matchers

class CoreUARTSimTest extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "CPU load from UART"

  def word_transmit(cpu: CoreTop, data: UInt, rcv_time: Int, hold: Int): Unit = {
    require(data.getWidth <= 32)
    for (i <-0 until 4) {
//      val base = (4-i)*8-1
      byte_transmit(cpu, data(i*8+7,i*8), rcv_time, hold)//TODO 小端序！
      println(s"----//////----------$i transmitted--------//////-----")
    }
  }

  def byte_transmit(cpu: CoreTop, data: UInt, rcv_time: Int, hold: Int): Unit = {
    require(data.getWidth == 8)
    cpu.io.external.uart.rxValid.poke(false.B)
    cpu.clock.step(rcv_time)
    cpu.io.external.uart.rxData.poke(data)
    cpu.io.external.uart.rxValid.poke(true.B)
    cpu.clock.step(hold)
  }

  it should "correctly load data from UART and stored into Mem" in {
    test(new CoreTop) { cpu =>
      cpu.io.external_signal.load_data_mode.poke(true.B)
      word_transmit(cpu, "h00a0_0093".U, 3, 3) // transmit addi x1,x0,10
      cpu.io.external_signal.load_data_mode.poke(false.B)
      run_instructions(cpu)
      checkRegsInCPU(cpu, 1, 10.U)
    }
  }
}
