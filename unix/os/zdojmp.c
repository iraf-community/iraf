/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#define import_spp
#define	import_kernel
#define	import_knames
#include <iraf.h>

/* ZDOJMP -- Restore the saved processor context (non-local goto).  See also
 * as$zsvjmp.s, where most of the work is done.
 */
ZDOJMP (jmpbuf, status)
XINT	*jmpbuf;
XINT	*status;
{
	register int stat = *status ? *status : 1;
	register long *jb = (long *)jmpbuf;

	*((int *)jb[0]) = stat;
	longjmp (&jb[1], stat);
}
