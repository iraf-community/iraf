/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_knames
#include <iraf.h>

/* ACLRB -- Clear a block of memory.
 */
ACLRB (a, nbytes)
XCHAR	*a;
XINT	*nbytes;
{
	register char	*p;
	register int	n;

	for (p=(char *)a, n = *nbytes;  --n >= 0;  )
	    *p++ = 0;
}
