@echo off
REM set up a CVS tree to build when there's no autotools
REM $Revision: 1.5 $
REM $Date: 2008-03-31 12:09:43 $

REM create hugehelp.c
copy src\hugehelp.c.cvs src\hugehelp.c

REM create Makefile
copy Makefile.dist Makefile
