package core.pipeline
import chisel3._
import chisel3.util._
import core.config.{BranchType, ForwardType, LayerControlSignal, NextPCControlSignal, ResultStageType}
class ConflictController extends Module{
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
    val IDEX_csr_to_write = Input(UInt(12.W))//csr always result at EX
    val IDEX_write_csr = Input(Bool())
    val IDEX_reg_result_stage = Input(ResultStageType.getWidth)
    //data in MEM stage
    val EXMEM_reg_to_write = Input(UInt(5.W))
    val EXMEM_write_reg = Input(Bool())
    val EXMEM_csr_to_write = Input(UInt(12.W))//csr always result at EX
    val EXMEM_write_csr = Input(Bool())
    val EXMEM_reg_result_stage = Input(ResultStageType.getWidth)


    //forward signal(ValForwardController use it to decide forwarding
    val rs1_forward_type = Output(ForwardType.getWidth)
    val rs2_forward_type = Output(ForwardType.getWidth)
    val csr_forward_type = Output(ForwardType.getWidth)


    //control hazard input check signal,input all from ID-EX and EX stage
    val branch_type =Input(BranchType.getWidth)
    val pc = Input(UInt(32.W))
    val imm = Input(UInt(32.W))
    val alu_result = Input(UInt(32.W))//for jalr use
    val cmp_result = Input(Bool())//for branch use
    val predict_next_pc = Input(UInt(32.W))//from IF-ID pc,only useful when is_branch!=not

    //exception hazard handling
    val exception_occurs = Input(Bool())
    val exception_new_pc = Input(UInt(32.W))

    //pc control
    val new_pc = Output(UInt(32.W))
    val next_control_signal = Output(NextPCControlSignal.getWidth)

    //layer control
    val IFID_control_signal =Output(LayerControlSignal.getWidth)
    val IDEX_control_signal =Output(LayerControlSignal.getWidth)
    val EXMEM_control_signal =Output(LayerControlSignal.getWidth)
    val MEMWB_control_signal =Output(LayerControlSignal.getWidth)
  })
  def assign_output():Unit={

  }
  //default output


  //TODO conflict controller
  val previous_no_hazard = true.B
  //uard loader
  when(io.uart_loading){
    previous_no_hazard:=false.B
    ///给所有夹层发STALL，TODO 注意global state machine也不嫩更新


  }
  //exception hazard
  when(io.exception_occurs){
    previous_no_hazard:=false.B

    /////

  }

  //control hazard
  when(previous_no_hazard){
    val hazard_occurs = false.B
    switch(io.branch_type){
      is(BranchType.BType.getUInt){
        //what is hazard

      }
      is(BranchType.JALR.getUInt){
        ///
      }
    }
    when(hazard_occurs){
      previous_no_hazard:=false.B
      //TODO
    }
  }
  //data hazard
  when(previous_no_hazard){
    //两个rs1 use,前递EX的
    when(io.IDEX_write_reg
      &&io.IDEX_reg_to_write=/=0.U
      &&io.rs1_use===io.IDEX_reg_to_write){
      previous_no_hazard:=false.B
      //检查结果的EX得到还是MEM得到
    }.elsewhen(io.EXMEM_write_reg
      &&io.EXMEM_reg_to_write=/=0.U
      &&io.rs1_use===io.EXMEM_reg_to_write){
      previous_no_hazard:=false.B
      //直接前递即可
    }
    //rs2,csr
  }

}
