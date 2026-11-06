@echo off
echo 这是一个不编译.c文件的版本（直接基于riscv汇编代码编译），请提供同名的.s文件
set /p fileName=请输入文件名（默认main.c）：
if "%fileName%"=="" set fileName=main.c
:: 预处理
:: 编译
riscv-none-elf-as -march=rv32i -o %fileName:.c=.o% %fileName:.c=.s%
riscv-none-elf-ld -nostdlib -T linker_script.ld -o %fileName:.c=.elf% %fileName:.c=.o%
riscv-none-elf-objcopy -O binary %fileName:.c=.elf% %fileName:.c=.bin%
hexdump -v -e "1/4 ""%%08x\n""" %fileName:.c=.bin% > %fileName:.c=.txt%
echo 完成汇编至：%fileName:.c=.elf%
echo 继续可查看反汇编代码
pause
:: 反汇编代码
riscv-none-elf-objdump -D -b binary -m riscv:rv32 %fileName:.c=.bin% 
pause