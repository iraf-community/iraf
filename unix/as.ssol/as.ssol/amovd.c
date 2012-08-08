/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_knames
#include <iraf.h>

/* AMOVD -- Copy a block of memory.
 * [Specially optimized for Sun/IRAF].
 */
AMOVD (a, b, n)
XDOUBLE	*a, *b;
XINT	*n;
{
	if (a != b)
	    memmove ((char *)b, (char *)a, *n * sizeof(*a));
}
