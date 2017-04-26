/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include "ratdef.h"

FINT
GETARG(n, s, maxsiz)
FINT *n;
register RCHAR *s;
FINT *maxsiz;
{
	extern int xargc;
	extern char **xargv;
	register char *t;
	register int i;

	if(*n>=0 && *n<xargc)
		t = xargv[*n];
	else if (*n == -1)
		return(xargc);
	else
		return(REOF);	/* non-existent argument */

	for(i = 0; i<*maxsiz-1 && *t!='\0' ; ++i)
		*s++ = *t++;
	*s++ = REOS;	/* terminate ratfor string with eos */
	return(i);	/* return length of argument */
}
