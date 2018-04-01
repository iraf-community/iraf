/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_knames
#include <iraf.h>

/* AMOVR -- Copy a block of memory.
 * [Specially optimized for Sun/IRAF].
 */
AMOVR (
  XREAL	*a,
  XREAL	*b,
  XINT	*n
)
{
	if (a != b)
	    memmove ((char *)b, (char *)a, *n * sizeof(*a));
}
