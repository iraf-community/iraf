/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <setjmp.h>

#include <stdio.h>
#define import_spp
#define	import_kernel
#define	import_knames
#include <iraf.h>

/* ZDOJMP -- Restore the saved processor context (non-local goto).  See also
 * as$zsvjmp.s, where most of the work is done.
 */
void
ZDOJMP (XINT *jmpbuf, XINT *status)
{
#ifdef DOJMP_ORIG
	register int stat = *status ? *status : 1;
	register long *jb = (long *)jmpbuf;

	*((int *)jb[0]) = stat;
	longjmp (&jb[1], stat);

#else
	register int stat = *status ? *status : 1;
        register XINT *status_ptr = ((XINT **)jmpbuf)[0];
        register void *jb = (XINT **)jmpbuf+1;

        *status_ptr = stat;
#if (defined(LINUX) || defined(CYGWIN))
        siglongjmp (jb, stat);
#else
        longjmp (jb, stat);
#endif

#endif
}
