/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_knames
#include <iraf.h>

/* AMOVR -- Copy a block of memory.
 * [Specially optimized for Sun/IRAF].
 */
AMOVR (a, b, n)
XREAL	*a, *b;
XINT	*n;
{
	bcopy ((char *)a, (char *)b, *n * sizeof(*a));
}
