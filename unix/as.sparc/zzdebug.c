/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#define import_spp
#define	import_kernel
#define	import_knames
#include <iraf.h>

/*
 * ZZDEBUG -- Test program for ZSVJMP/ZDOJMP.  Will return "exit status 1"
 * if it runs successfully.
 */


int	jmpbuf[LEN_JUMPBUF];
int	status;

main()
{
	zsvjmp_((char *)jmpbuf, &status);
	if (status) {
	    printf ("exit status %d\n", status);
	    exit (status);
	}

	a(1);
	exit (0);
}


a(status)
int	status;
{
	ZDOJMP(jmpbuf, &status);
}


/* ZDOJMP -- Restore the saved processor context (non-local goto).  See also
 * as$zsvjmp.s, where most of the work is done.
 */
ZDOJMP (jmpbuf, status)
XINT	*jmpbuf;
XINT	*status;
{
	*((int *)jmpbuf[0]) = *status;
	longjmp (&jmpbuf[1], *status);
}
