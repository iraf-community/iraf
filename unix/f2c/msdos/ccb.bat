rem script for compiling f2c with Borland C++ 4.02
del *.obj
bcc -c -ml -N -w-pia -w-pro -O2 -Ot -Ox -G -Z -Oe cds.c >zot
if errorlevel 1 goto
bcc -c -ml -N -w-pia -w-pro -O2 -Ot -Ox -G -Z -Oe data.c >zot
if errorlevel 1 goto
bcc -c -ml -N -w-pia -w-pro -O2 -Ot -Ox -G -Z -Oe equiv.c >zot
if errorlevel 1 goto
bcc -c -ml -N -w-pia -w-pro -O2 -Ot -Ox -G -Z -Oe error.c >zot
if errorlevel 1 goto
bcc -c -ml -N -w-pia -w-pro -O2 -Ot -Ox -G -Z -Oe exec.c >zot
if errorlevel 1 goto
bcc -c -ml -N -w-pia -w-pro -O2 -Ot -Ox -G -Z -Oe expr.c >zot
if errorlevel 1 goto
bcc -c -ml -N -w-pia -w-pro -O2 -Ot -Ox -G -Z -Oe format.c >zot
if errorlevel 1 goto
bcc -c -ml -N -w-pia -w-pro -O2 -Ot -Ox -G -Z -Oe formatda.c >zot
if errorlevel 1 goto
bcc -c -ml -N -w-pia -w-pro -O2 -Ot -Ox -G -Z -Oe gram.c >zot
if errorlevel 1 goto
bcc -c -ml -N -w-pia -w-pro -O2 -Ot -Ox -G -Z -Oe init.c >zot
if errorlevel 1 goto
bcc -c -ml -N -w-pia -w-pro -O2 -Ot -Ox -G -Z -Oe intr.c >zot
if errorlevel 1 goto
bcc -c -ml -N -w-pia -w-pro -O2 -Ot -Ox -G -Z -Oe io.c >zot
if errorlevel 1 goto
bcc -c -ml -N -w-pia -w-pro -O2 -Ot -Ox -G -Z -Oe lex.c >zot
if errorlevel 1 goto
bcc -c -ml -N -w-pia -w-pro -O2 -Ot -Ox -G -Z -Oe main.c >zot
if errorlevel 1 goto
bcc -c -ml -N -w-pia -w-pro -O2 -Ot -Ox -G -Z -Oe mem.c >zot
if errorlevel 1 goto
bcc -c -ml -N -w-pia -w-pro -O2 -Ot -Ox -G -Z -Oe misc.c >zot
if errorlevel 1 goto
bcc -c -ml -N -w-pia -w-pro -O2 -Ot -Ox -G -Z -Oe names.c >zot
if errorlevel 1 goto
bcc -c -ml -N -w-pia -w-pro -O2 -Ot -Ox -G -Z -Oe niceprin.c >zot
if errorlevel 1 goto
bcc -c -ml -N -w-pia -w-pro -O2 -Ot -Ox -G -Z -Oe output.c >zot
if errorlevel 1 goto
bcc -c -ml -N -w-pia -w-pro -O2 -Ot -Ox -G -Z -Oe p1output.c >zot
if errorlevel 1 goto
bcc -c -ml -N -w-pia -w-pro -O2 -Ot -Ox -G -Z -Oe parse_ar.c >zot
if errorlevel 1 goto
bcc -c -ml -N -w-pia -w-pro -O2 -Ot -Ox -G -Z -Oe pread.c >zot
if errorlevel 1 goto
bcc -c -ml -N -w-pia -w-pro -O2 -Ot -Ox -G -Z -Oe proc.c >zot
if errorlevel 1 goto
bcc -c -ml -N -w-pia -w-pro -O2 -Ot -Ox -G -Z -Oe put.c >zot
if errorlevel 1 goto
bcc -c -ml -N -w-pia -w-pro -O2 -Ot -Ox -G -Z -Oe putpcc.c >zot
if errorlevel 1 goto
bcc -c -ml -N -w-pia -w-pro -O2 -Ot -Ox -G -Z -Oe sysdep.c >zot
if errorlevel 1 goto
bcc -c -ml -N -w-pia -w-pro -O2 -Ot -Ox -G -Z -Oe vax.c >zot
if errorlevel 1 goto
bcc -c -ml -N -w-pia -w-pro -O2 -Ot -Ox -G -Z -Oe version.c >zot
if errorlevel 1 goto
echo extern unsigned _stklen = 0x4000; >stklen.c
bcc -c -ml -N -w-pia -w-pro -O2 -Ot -Ox -G -Z -Oe stklen.c >zot
if errorlevel 1 goto
bcc -ml -N -ef2c *.obj
if errorlevel 1 goto
del *.obj
