package core.pipeline

import chisel3._
import chisel3.util._
import core.config.{BranchType, ForwardType, LayerControlSignal, NextPCControlSignal, ResultStageType}

class ConflictController extends Module {
  val io = IO(new Bundle() {
    //stall all
    val uart_loading = Input(Bool())
    //data hazard input check signal
    val rs1_use = Input(UInt(5.W))
    val rs2_use = Input(UInt(5.W))
    val csr_use = Input(UInt(12.W))
    //data in EX stage
    val IDEX_reg_to_write = Input(UInt(5.W))
    val IDEX_write_reg = Input(Bool())
    val IDEX_csr_to_write = Input(UInt(12.W)) //csr always result at EX
    val IDEX_write_csr = Input(Bool())
    val IDEX_reg_result_stage = Input(ResultStageType.getWidth)
    //data in MEM stage
    val EXMEM_reg_to_write = Input(UInt(5.W))
    val EXMEM_write_reg = Input(Bool())
    val EXMEM_csr_to_write = Input(UInt(12.W)) //csr always result at EX
    val EXMEM_write_csr = Input(Bool())
    val EXMEM_reg_result_stage = Input(ResultStageType.getWidth)


    //forward signal(ValForwardController use it to decide forwarding
    val rs1_forward_type = Output(ForwardType.getWidth)
    val rs2_forward_type = Output(ForwardType.getWidth)
    val csr_forward_type = Output(ForwardType.getWidth)


    //control hazard input check signal,input all from ID-EX and EX stage
    val branch_type = Input(BranchType.getWidth)
    val pc = Input(UInt(32.W))
    val imm = Input(UInt(32.W))
    val alu_result = Input(UInt(32.W)) //for jalr use
    val cmp_result = Input(Bool()) //for branch use
    val predict_next_pc = Input(UInt(32.W)) //from IF-ID pc,only useful when is_branch!=not

    //exception hazard handling
    val exception_occurs = Input(Bool())
    val exception_new_pc = Input(UInt(32.W))

    //pc control
    val new_pc = Output(UInt(32.W))
    val next_control_signal = Output(NextPCControlSignal.getWidth)

    //layer control
    val IFID_control_signal = Output(LayerControlSignal.getWidth)
    val IDEX_control_signal = Output(LayerControlSignal.getWidth)
    val EXMEM_control_signal = Output(LayerControlSignal.getWidth)
    val MEMWB_control_signal = Output(LayerControlSignal.getWidth)
  })

  def assign_output(rs1_forward_type: UInt = ForwardType.No.getUInt,
                    rs2_forward_type: UInt = ForwardType.No.getUInt,
                    csr_forward_type: UInt = ForwardType.No.getUInt,
                    new_pc: UInt = 0.U,
                    nextpc_control_signal: UInt = NextPCControlSignal.Normal.getUInt,
                    IFID_control_signal: UInt = LayerControlSignal.Normal.getUInt,
                    IDEX_control_signal: UInt = LayerControlSignal.Normal.getUInt,
                    EXMEM_control_signal: UInt = LayerControlSignal.Normal.getUInt,
                    MEMWB_control_signal: UInt = LayerControlSignal.Normal.getUInt
                   ): Unit = {
    io.rs1_forward_type := rs1_forward_type
    io.rs2_forward_type := rs2_forward_type
    io.csr_forward_type := csr_forward_type
    io.new_pc := new_pc
    io.next_control_signal := nextpc_control_signal
    io.IFID_control_signal := IFID_control_signal
    io.IDEX_control_signal := IDEX_control_signal
    io.EXMEM_control_signal := EXMEM_control_signal
    io.MEMWB_control_signal := MEMWB_control_signal
  }

  assign_output() //default output, no hazard

  //TODO conflict controller
  var previous_no_hazard = true
  //uard loader
  when(io.uart_loading) {
    previous_no_hazard =false
    ///给所有夹层发STALL，TODO 注意global state machine也不能更新
    assign_output(
      nextpc_control_signal = NextPCControlSignal.Stall.getUInt,
      IFID_control_signal = LayerControlSignal.Stall.getUInt,
      IDEX_control_signal = LayerControlSignal.Stall.getUInt,
      EXMEM_control_signal = LayerControlSignal.Stall.getUInt,
      MEMWB_control_signal = LayerControlSignal.Stall.getUInt
    )
  }
  //exception hazard
  when(io.exception_occurs) {
    previous_no_hazard =false
    assign_output(
      new_pc = io.exception_new_pc,
      IFID_control_signal = LayerControlSignal.NOP.getUInt,
      IDEX_control_signal = LayerControlSignal.NOP.getUInt,
      EXMEM_control_signal = LayerControlSignal.NOP.getUInt,
    )
  }

  //control hazard
  if(previous_no_hazard) {
    switch(io.branch_type) {
      is(BranchType.BType.getUInt) {
        val correct_pc = io.pc + io.imm
        when(io.cmp_result
          && io.predict_next_pc =/= correct_pc) {
          previous_no_hazard =false
          assign_output(
            new_pc = correct_pc,
            IFID_control_signal = LayerControlSignal.NOP.getUInt,
            IDEX_control_signal = LayerControlSignal.NOP.getUInt
          )
        }
      }
      is(BranchType.JALR.getUInt) {
        val correct_pc = io.alu_result
        when(io.predict_next_pc =/= correct_pc) {
          previous_no_hazard =false
          assign_output(
            new_pc = correct_pc,
            IFID_control_signal = LayerControlSignal.NOP.getUInt,
            IDEX_control_signal = LayerControlSignal.NOP.getUInt
          )
        }
      }
    }
  }
  //data hazard
  if(previous_no_hazard) {
    //两个rs1 use,前递EX的
    when(io.IDEX_write_reg
      && io.IDEX_reg_to_write =/= 0.U
      && io.rs1_use === io.IDEX_reg_to_write) {
      previous_no_hazard =false
      when(io.IDEX_reg_result_stage === ResultStageType.EX.getUInt) { //检查结果的EX得到还是MEM得到
        assign_output(
          rs1_forward_type = ForwardType.EXForward.getUInt
        )
      }.otherwise { //MEM
        assign_output(
          nextpc_control_signal = NextPCControlSignal.Stall.getUInt,
          IDEX_control_signal = LayerControlSignal.NOP.getUInt,
          IFID_control_signal = LayerControlSignal.Stall.getUInt,
        )
      }
    }.elsewhen(io.EXMEM_write_reg
      && io.EXMEM_reg_to_write =/= 0.U
      && io.rs1_use === io.EXMEM_reg_to_write) { //直接前递即可
      previous_no_hazard =false
      assign_output(
        rs1_forward_type = ForwardType.EXForward.getUInt
      )
    }
    //rs2
    when(io.IDEX_write_reg
      && io.IDEX_reg_to_write =/= 0.U
      && io.rs2_use === io.IDEX_reg_to_write) {
      previous_no_hazard =false
      when(io.IDEX_reg_result_stage === ResultStageType.EX.getUInt) { //检查结果的EX得到还是MEM得到
        assign_output(
          rs2_forward_type = ForwardType.EXForward.getUInt
        )
      }.otherwise { //MEM
        assign_output(
          nextpc_control_signal = NextPCControlSignal.Stall.getUInt,
          IDEX_control_signal = LayerControlSignal.NOP.getUInt,
          IFID_control_signal = LayerControlSignal.Stall.getUInt,
        )
      }
    }.elsewhen(io.EXMEM_write_reg
      && io.EXMEM_reg_to_write =/= 0.U
      && io.rs2_use === io.EXMEM_reg_to_write) { //直接前递即可
      previous_no_hazard =false
      assign_output(
        rs2_forward_type = ForwardType.EXForward.getUInt
      )
    }
    //csr
    when(io.IDEX_write_csr
      && io.IDEX_csr_to_write =/= 0.U
      && io.csr_use === io.IDEX_csr_to_write) {
      previous_no_hazard =false
      when(io.IDEX_reg_result_stage === ResultStageType.EX.getUInt) { //检查结果的EX得到还是MEM得到
        assign_output(
          csr_forward_type = ForwardType.EXForward.getUInt
        )
      }.otherwise { //MEM
        assign_output(
          nextpc_control_signal = NextPCControlSignal.Stall.getUInt,
          IDEX_control_signal = LayerControlSignal.NOP.getUInt,
          IFID_control_signal = LayerControlSignal.Stall.getUInt,
        )
      }
    }.elsewhen(io.EXMEM_write_csr
      && io.EXMEM_csr_to_write =/= 0.U
      && io.csr_use === io.EXMEM_csr_to_write) { //直接前递即可
      previous_no_hazard =false
      assign_output(
        csr_forward_type = ForwardType.EXForward.getUInt
      )
    }
  }
}
