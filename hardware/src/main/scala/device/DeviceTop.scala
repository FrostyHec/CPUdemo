package device
import chisel3._
class MMIOOutBundle extends Bundle{
  //cpu output devices
  val led=new MMIOLedBundle
  val seg7=new MMIOSeg7Bundle

  //cpu input devices
  val btn=new MMIOButtonBundle
  val switch=new MMIOSwitchBundle
}
class BoardBundle extends Bundle{
  //output devices
  val led=new BoardLedBundle
  val seg7=new BoardSeg7Bundle

  //input devices
  val btn=new BoardButtonBundle
  val switch=new BoardSwitchBundle
}
class DeviceTop extends Module{
  val io=IO(new Bundle{
    val mmio=new MMIOOutBundle
    val board=new BoardBundle
  })
  val led=new Led
  val seg7=new Seg7
  val btn=new Button
  val switch=new Switch
  //mmio
  led.io.mmio<>io.mmio.led
  seg7.io.mmio<>io.mmio.seg7
  btn.io.mmio<>io.mmio.btn
  switch.io.mmio<>io.mmio.switch

  //board
  led.io.board<>io.board.led
  seg7.io.board<>io.board.seg7
  btn.io.board<>io.board.btn
  switch.io.board<>io.board.switch
}
