/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_knames
#include <iraf.h>

/* ACLRD -- Clear a block of memory.
 * [Specially optimized for Sun/IRAF].
 */
ACLRD (a, n)
XDOUBLE	*a;
XINT	*n;
{
	bzero ((char *)a, *n * sizeof(*a));
}
