/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <setjmp.h>

#define import_spp
#define import_kernel
#define import_knames
#include <iraf.h>

extern int zsvjmp_ ( XINT *, XINT * );

/*
 * ZZDEBUG -- Test program for ZSVJMP/ZDOJMP.  Will return "exit status 1"
 * if it runs successfully.
 */


XINT jmpbuf[LEN_JUMPBUF];
XINT status;

void a ( XINT );
void ZDOJMP ( XINT *, XINT * );

int main( int argc, char *argv[] )
{
	zsvjmp_(jmpbuf, &status);
	if (status) {
	    printf ("exit status %ld\n", (long)status);
	    return status;
	}

	a(1);
	return 0;
}


void a( XINT status )
{
	ZDOJMP(jmpbuf, &status);
}


/* ZDOJMP -- Restore the saved processor context (non-local goto).  See also
 * as$zsvjmp.s, where most of the work is done.
 */
void ZDOJMP ( XINT *jmpbuf, XINT *status )
{
	register XINT stat = *status ? *status : 1;
	register XINT *status_ptr = ((XINT **)jmpbuf)[0];
	register void *jb = (XINT **)jmpbuf+1;

	*status_ptr = stat;
#ifdef LINUX
	siglongjmp (jb, stat);
#else
	longjmp (jb, stat);
#endif
}
