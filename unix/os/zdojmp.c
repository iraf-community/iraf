/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#define import_spp
#define	import_kernel
#define	import_knames
#ifdef A88K
#define import_setjmp
#endif
#include <iraf.h>

/* ZDOJMP -- Restore the saved processor context (non-local goto).  See also
 * as$zsvjmp.s, where most of the work is done.
 */
ZDOJMP (jmpbuf, status)
XINT	*jmpbuf;
XINT	*status;
{
	register int stat = *status;

#ifdef A88K
	int	*op;
	op = jmpbuf;
	op = op + LEN_JUMPBUF -1;
	*op = *status;
	longjmp (&jmpbuf[0], *status);
#else
	*((int *)jmpbuf[0]) = stat ? stat : 1;
	longjmp (&jmpbuf[1], *status);
#endif
}
