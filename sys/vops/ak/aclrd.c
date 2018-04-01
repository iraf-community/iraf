/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_knames
#include <iraf.h>

/* ACLRD -- Clear a block of memory.
 * [Specially optimized for Sun/IRAF].
 */
ACLRD (
  XDOUBLE	*a,
  XINT	*n
)
{
	memset ((char *)a, 0, *n * sizeof(*a));
}
