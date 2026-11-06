@echo off
echo 这是一个反汇编bin文件到riscv文件的代码
set /p fileName=请输入文件名（默认main.bin）：
if "%fileName%"=="" set fileName=main.bin
:: 查看反汇编结果
riscv-none-elf-objdump -D -b binary -m riscv:rv32 %fileName% > disassembly.txt
pause