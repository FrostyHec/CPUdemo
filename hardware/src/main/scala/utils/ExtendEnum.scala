package utils

import chisel3._
import chisel3.util.log2Ceil

// 扩展Enumeration类，添加getWidth方法
abstract class ExtendEnum extends Enumeration {
  // 自动计算并返回所需的最小位宽
  def getWidth = UInt(log2Ceil(this.maxId).W)

  implicit class RichEnumValue(val value: this.Value) {
    def getUInt = value.id.U
  }
}

