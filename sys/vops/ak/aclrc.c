/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <string.h>

#define import_spp
#define import_knames
#include <iraf.h>

/* ACLRC -- Clear a block of memory.
 * [Specially optimized for Sun/IRAF].
 */
void
ACLRC (
  XCHAR	*a,
  XINT	*n
)
{
	memset ((char *)a, 0, *n * sizeof(*a));
}
