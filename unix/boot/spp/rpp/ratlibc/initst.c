/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include "ratdef.h"

FILE	*_fdtofile[10] = {
		&_iob[0],	/* standard input */
		&_iob[1],	/* standard output */
		&_iob[2]	/* error output */
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
