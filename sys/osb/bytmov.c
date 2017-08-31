/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define	import_spp
#define import_knames
#include <iraf.h>

/* BYTMOV -- Byte move from array "a" to array "b".  The move must be
 * nondestructive, allowing a byte array to be shifted left or right a
 * few bytes, hence comparison of the addresses of the arrays is necessary
 * to determine if they overlap.
 * [Specially optimized version for Sun/IRAF].
 */
BYTMOV (
  XCHAR	*a,			/* input byte array			*/
  XINT	*aoff,			/* first byte in A to be moved		*/
  XCHAR	*b,			/* output byte array			*/
  XINT	*boff,			/* first byte in B to be written	*/
  XINT	*nbytes 		/* number of bytes to move		*/
)
{
	if ((a + *aoff) != (b + *boff))
	    memmove ((char *)b + (*boff-1), (char *)a + (*aoff-1), *nbytes);
}
