rem script for compiling f2cx (extended-memory f2c) with Symantec C version 6
del *.obj
sc -c -s -mx -o -w2 -w7 -DMSDOS cds.c >zot
if errorlevel 1 goto
sc -c -s -mx -o -w2 -w7 -DMSDOS data.c >>zot
if errorlevel 1 goto
sc -c -s -mx -o -w2 -w7 -DMSDOS equiv.c >>zot
if errorlevel 1 goto
sc -c -s -mx -o -w2 -w7 -DMSDOS error.c >>zot
if errorlevel 1 goto
sc -c -s -mx -o -w2 -w7 -DMSDOS exec.c >>zot
if errorlevel 1 goto
sc -c -s -mx -o -w2 -w7 -DMSDOS expr.c >>zot
if errorlevel 1 goto
sc -c -s -mx -o -w2 -w7 -DMSDOS format.c >>zot
if errorlevel 1 goto
sc -c -s -mx -o -w2 -w7 -DMSDOS formatda.c >>zot
if errorlevel 1 goto
sc -c -s -mx -o -w2 -w7 -DMSDOS gram.c >>zot
if errorlevel 1 goto
sc -c -s -mx -o -w2 -w7 -DMSDOS init.c >>zot
if errorlevel 1 goto
sc -c -s -mx -o -w2 -w7 -DMSDOS intr.c >>zot
if errorlevel 1 goto
sc -c -s -mx -o -w2 -w7 -DMSDOS io.c >>zot
if errorlevel 1 goto
sc -c -s -mx -o -w2 -w7 -DMSDOS lex.c >>zot
if errorlevel 1 goto
sc -c -s -mx -o -w2 -w7 -DMSDOS main.c >>zot
if errorlevel 1 goto
sc -c -s -mx -o -w2 -w7 -DMSDOS mem.c >>zot
if errorlevel 1 goto
sc -c -s -mx -o -w2 -w7 -DMSDOS misc.c >>zot
if errorlevel 1 goto
sc -c -s -mx -o -w2 -w7 -DMSDOS names.c >>zot
if errorlevel 1 goto
sc -c -s -mx -o -w2 -w7 -DMSDOS niceprin.c >>zot
if errorlevel 1 goto
sc -c -s -mx -o -w2 -w7 -DMSDOS -DUSE_DTOA output.c >>zot
if errorlevel 1 goto
sc -c -s -mx -o -w2 -w7 -DMSDOS p1output.c >>zot
if errorlevel 1 goto
sc -c -s -mx -o -w2 -w7 -DMSDOS parse_ar.c >>zot
if errorlevel 1 goto
sc -c -s -mx -o -w2 -w7 -DMSDOS pread.c >>zot
if errorlevel 1 goto
sc -c -s -mx -o -w2 -w7 -DMSDOS proc.c >>zot
if errorlevel 1 goto
sc -c -s -mx -o -w2 -w7 -DMSDOS put.c >>zot
if errorlevel 1 goto
sc -c -s -mx -o -w2 -w7 -DMSDOS putpcc.c >>zot
if errorlevel 1 goto
sc -c -s -mx -o -w2 -w7 -DMSDOS sysdep.c >>zot
if errorlevel 1 goto
sc -c -s -mx -o -w2 -w7 -DMSDOS vax.c >>zot
if errorlevel 1 goto
sc -c -s -mx -o -w2 -w7 -DMSDOS version.c >>zot
if errorlevel 1 goto
rem The following echo and ren create stklen.c if it does not exist
rem and avoid overwriting an existing stklen.c .
echo extern unsigned _stklen = 0x4000; >zap
ren zap stklen.c
sc -c -s -mx -o -w2 -w7 -DMSDOS stklen.c >>zot
if errorlevel 1 goto
rem README tells about dtoa.c and g_fmt.c .
sc -c -s -mx -o -w2 -w7 -DMSDOS -DMALLOC=ckalloc -DIEEE_8087 dtoa.c >>zot
if errorlevel 1 goto
sc -c -s -mx -o -w2 -w7 -DMSDOS -DIEEE_8087 g_fmt.c >>zot
if errorlevel 1 goto
sc -mx -s -o f2cx.exe *.obj
del *.obj
