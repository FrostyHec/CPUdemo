from pyverilator import PyVerilator
from unicorn import *
from unicorn.riscv_const import *
# Main 2就是整个测试平台
# 需要做的事情：依据项目进一步封装这个平台

# Load the Verilog module
sim = PyVerilator.build('SimTop.v')

# Initialize Unicorn emulator
uc = Uc(UC_ARCH_RISCV, UC_MODE_RISCV64)

# Load RISC-V code into Unicorn memory
CODE = b"\x13\x00\x00\x00" # RISC-V instruction for "addi a0, x0, 0"
uc.mem_map(0x1000, 0x1000)
uc.mem_write(0x1000, CODE)

# Reset the Verilog simulation
sim.reset()

# Run the RISC-V instruction in both the Verilog simulation and Unicorn emulator
sim.io_instr_valid = 1
sim.io_instr = 0x1000
sim.io_rst_n = 1
sim.io_clk = 0

sim.io_clk = 1
while not sim.io_instr_ready:
    sim.io_clk = 0
    sim.io_clk = 1

uc.reg_write(UC_RISCV_REG_X0, 0) # Set x0 to 0
uc.emu_start(0x1000, 0x1000)

# Compare the values of the registers after executing the instruction
verilog_a0 = sim.io_reg_a0
unicorn_a0 = uc.reg_read(UC_RISCV_REG_X10) # a0 is x10 in Unicorn

if verilog_a0 == unicorn_a0:
    print("Both Verilog simulation and Unicorn emulator produced the same result.")
else:
    print("Difference found in register a0: Verilog={}, Unicorn={}".format(verilog_a0, unicorn_a0))