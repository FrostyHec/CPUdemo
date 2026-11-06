@echo off
set /p fileName=请输入文件名（默认main.c）：
if "%fileName%"=="" set fileName=main.c
:: 预处理
riscv-none-elf-gcc -E %fileName% -o %fileName:.c=.i%
:: 编译
riscv-none-elf-gcc -S -march=rv32i %fileName:.c=.i% -o %fileName:.c=.s%
echo 汇编代码输出至：%fileName:.c=.s%
pause
:: 汇编
riscv-none-elf-as -march=rv32i -o %fileName:.c=.o% %fileName:.c=.s%
:: 链接
riscv-none-elf-ld -nostdlib -T linker_script.ld -o %fileName:.c=.elf% %fileName:.c=.o%
riscv-none-elf-objcopy -O binary %fileName:.c=.elf% %fileName:.c=.bin%
hexdump -v -e "1/4 ""%%08x\n""" %fileName:.c=.bin% > %fileName:.c=.txt%
echo 完成汇编至：%fileName:.c=.elf%
pause