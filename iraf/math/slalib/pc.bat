@echo off
rem
rem  - - - - - - -
rem   P C . B A T
rem  - - - - - - -
rem
rem  Create the SLALIB library, starting either from the release
rem  version copied from the VAX or from the PC version produced
rem  by a prior run of the present procedure.
rem
rem  Environment variable temp must point to a scratch directory.
rem
rem  !!! Non-reversible - it is not possible to recreate !!!
rem  !!! the VAX version from the PC version             !!!
rem
rem  P.T.Wallace   Starlink   25 April 1996
rem
if exist *.pcm copy *.pcm *.for
if exist *.pcm del *.pcm
if exist *.c del *.c
if exist *.obj del *.obj
if exist *.exe del *.exe
if exist *.olb del *.olb
if exist *.vax del *.vax
if exist *.ush del *.ush
if exist *.cnv del *.cnv
if exist *.mip del *.mip
if exist *.sun del *.sun
if exist *.ind del *.ind
if exist *.com del *.com
if exist *.new del *.new
if exist *.obs del *.obs
if exist *.toc del *.toc
if exist *.dir del *.dir
if exist sla_test.for del sla_test.for
if exist makefile del makefile
if exist mk del mk
if exist sla_link del sla_link
if exist a*.for fl/c /FPi a*.for
if exist b*.for fl/c /FPi b*.for
if exist c*.for fl/c /FPi c*.for
if exist d*.for fl/c /FPi d*.for
if exist e*.for fl/c /FPi e*.for
if exist f*.for fl/c /FPi f*.for
if exist g*.for fl/c /FPi g*.for
if exist h*.for fl/c /FPi h*.for
if exist i*.for fl/c /FPi i*.for
if exist j*.for fl/c /FPi j*.for
if exist k*.for fl/c /FPi k*.for
if exist l*.for fl/c /FPi l*.for
if exist m*.for fl/c /FPi m*.for
if exist n*.for fl/c /FPi n*.for
if exist o*.for fl/c /FPi o*.for
if exist p*.for fl/c /FPi p*.for
if exist q*.for fl/c /FPi q*.for
if exist r*.for fl/c /FPi r*.for
if exist s*.for fl/c /FPi s*.for
if exist t*.for fl/c /FPi t*.for
if exist u*.for fl/c /FPi u*.for
if exist v*.for fl/c /FPi v*.for
if exist w*.for fl/c /FPi w*.for
if exist x*.for fl/c /FPi x*.for
if exist y*.for fl/c /FPi y*.for
if exist z*.for fl/c /FPi z*.for
echo Building library SLALIB.LIB ...
@echo slalib > %temp%\slalib.tmp
@echo y >> %temp%\slalib.tmp
for %%f in (*.obj) do echo +%%f& >> %temp%\slalib.tmp
@echo ;. >> %temp%\slalib.tmp
if exist slalib.lib del slalib.lib
lib @%temp%\slalib.tmp
del %temp%\slalib.tmp
del *.obj
echo ... done.
echo:
