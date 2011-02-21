rem script for compiling conventional-memory f2c with Microsoft C compilers
del *.obj
cl -c -AL -Gt28 -Ox -Ge -nologo CDS.C
if errorlevel 1 goto
del CDS.C
cl -c -AL -Gt28 -Ox -Ge -nologo DATA.C
if errorlevel 1 goto
del DATA.C
cl -c -AL -Gt28 -Ox -Ge -nologo EQUIV.C
if errorlevel 1 goto
del EQUIV.C
cl -c -AL -Gt28 -Ox -Ge -nologo ERROR.C
if errorlevel 1 goto
del ERROR.C
cl -c -AL -Gt28 -Ox -Ge -nologo EXEC.C
if errorlevel 1 goto
del EXEC.C
cl -c -AL -Gt28 -Ox -Ge -nologo EXPR.C
if errorlevel 1 goto
del EXPR.C
cl -c -AL -Gt28 -Ox -Ge -nologo FORMAT.C
if errorlevel 1 goto
del FORMAT.C
cl -c -AL -Gt28 -Ox -Ge -nologo FORMATDA.C
if errorlevel 1 goto
del FORMATDA.C
cl -c -AL -Gt28 -Ox -Ge -nologo GRAM.C
if errorlevel 1 goto
del GRAM.C
cl -c -AL -Gt28 -Ox -Ge -nologo INIT.C
if errorlevel 1 goto
del INIT.C
cl -c -AL -Gt28 -Ox -Ge -nologo INTR.C
if errorlevel 1 goto
del INTR.C
cl -c -AL -Gt28 -Ox -Ge -nologo IO.C
if errorlevel 1 goto
del IO.C
cl -c -AL -Gt28 -Ox -Ge -nologo LEX.C
if errorlevel 1 goto
del LEX.C
cl -c -AL -Gt28 -Ox -Ge -nologo MAIN.C
if errorlevel 1 goto
del MAIN.C
cl -c -AL -Gt28 -Ox -Ge -nologo MEM.C
if errorlevel 1 goto
del MEM.C
cl -c -AL -Gt28 -Ox -Ge -nologo MISC.C
if errorlevel 1 goto
del MISC.C
cl -c -AL -Gt28 -Ox -Ge -nologo NAMES.C
if errorlevel 1 goto
del NAMES.C
cl -c -AL -Gt28 -Ox -Ge -nologo NICEPRIN.C
if errorlevel 1 goto
del NICEPRIN.C
cl -c -AL -Gt28 -Ox -Ge -nologo OUTPUT.C
if errorlevel 1 goto
del OUTPUT.C
cl -c -AL -Gt28 -Ox -Ge -nologo P1OUTPUT.C
if errorlevel 1 goto
del P1OUTPUT.C
cl -c -AL -Gt28 -Ox -Ge -nologo PARSE_AR.C
if errorlevel 1 goto
del PARSE_AR.C
cl -c -AL -Gt28 -Ox -Ge -nologo PREAD.C
if errorlevel 1 goto
del PREAD.C
cl -c -AL -Gt28 -Ox -Ge -nologo PROC.C
if errorlevel 1 goto
del PROC.C
cl -c -AL -Gt28 -Ox -Ge -nologo PUT.C
if errorlevel 1 goto
del PUT.C
cl -c -AL -Gt28 -Ox -Ge -nologo PUTPCC.C
if errorlevel 1 goto
del PUTPCC.C
cl -c -AL -Gt28 -Ox -Ge -nologo SYSDEP.C
if errorlevel 1 goto
del SYSDEP.C
cl -c -AL -Gt28 -Ox -Ge -nologo VAX.C
if errorlevel 1 goto
del VAX.C
cl -c -AL -Gt28 -Ox -Ge -nologo VERSION.C
if errorlevel 1 goto
del VERSION.C
cl -AL *.obj -link /ST:0x6000
if errorlevel 1 goto
ren cds.exe f2c.exe
if errorlevel 1 goto
