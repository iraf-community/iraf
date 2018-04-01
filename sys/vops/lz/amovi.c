/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_knames
#include <iraf.h>

/* AMOVI -- Copy a block of memory.
 * [Specially optimized for Sun/IRAF].
 */
AMOVI (
  XINT	*a,
  XINT	*b,
  XINT	*n
)
{
	if (a != b)
	    memmove ((char *)b, (char *)a, *n * sizeof(*a));
}
