@echo off
set /p fileName=请输入所需要清理的文件类名（默认main）：
if "%fileName%"=="" set fileName=main
if exist %fileName%.i del %fileName%.i
if exist %fileName%.elf del %fileName%.elf
if exist %fileName%.txt del %fileName%.txt
if exist %fileName%.o del %fileName%.o
if exist %fileName%.bin del %fileName%.bin
if exist disassembly.txt del disassembly.txt
echo 删除完成
pause