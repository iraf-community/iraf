/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include "ratdef.h"

FILE	*_fdtofile[10];

/*
 * Ratfor initialization routine.  To be called as the first
 * executable statement of every program using the tools
 * subroutines.
 */
void 
INITST (void)
{
	_fdtofile[0] = stdin;
	_fdtofile[1] = stdout;
	_fdtofile[2] = stderr;
}
