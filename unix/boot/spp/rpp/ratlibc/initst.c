/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include "ratdef.h"

FILE	*_fdtofile[10] = {
		stdin,		/* standard input */
		stdout,		/* standard output */
		stderr		/* error output */
	};

/*
 * Ratfor initialization routine.  To be called as the first
 * executable statement of every program using the tools
 * subroutines.
 */
INITST()
{
	return;
}
