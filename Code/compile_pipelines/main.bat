@echo off

:menu
echo 欢迎来到编译小工具，本工具支持如下功能
echo [1] 十六进制.txt文件转.coe文件
echo [2] 大端序.txt文件转小端序
echo [3] 小端序.txt文件转大端序
echo [4] C代码riscv编译
echo [5] riscv汇编代码编译
echo [6] riscv机器码反汇编
echo [7] 清理编译文件
echo [8] 退出
echo 请输入编号以进入功能:
set /p input=

if "%input%"=="1" goto coe
if "%input%"=="2" goto endian
if "%input%"=="3" goto endian
if "%input%"=="4" goto C_com
if "%input%"=="5" goto A_com
if "%input%"=="6" goto dis
if "%input%"=="7" goto Clean
if "%input%"=="8" goto exit
clear
echo 输入错误！
goto menu


:coe
set /p fileName=请输入所需转换的文件名称（默认main.txt）：
if "%fileName%"=="" set fileName=main.txt
python %~dp0coe_generator.py "%fileName%"
echo 转换成功
echo 文件名为: "%fileName:.txt=.coe%"
pause
goto menu

:endian
set /p fileName=请输入所需转换的文件名称（默认main.txt）：
if "%fileName%"=="" set fileName=main.txt
python %~dp0LittleEndian.py "%fileName%"
echo 转换成功
echo 文件名为: "%fileName:.txt=_out.txt%"
pause
goto menu

:C_com
call get_riscv.bat
pause
goto menu

:A_com
call get_riscv_from_assemble.bat
pause
goto menu

:dis
call disassemble.bat
pause
goto menu

:Clean
call clean.bat
pause
goto menu

:exit
