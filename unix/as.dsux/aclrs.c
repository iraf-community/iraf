/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_knames
#include <iraf.h>

/* ACLRS -- Clear a block of memory.
 * [Specially optimized for Sun/IRAF].
 */
ACLRS (a, n)
XSHORT	*a;
XINT	*n;
{
	bzero ((char *)a, *n * sizeof(*a));
}
