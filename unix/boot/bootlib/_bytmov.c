/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define	import_spp
#define import_knames
#include <iraf.h>

/* BYTMOV -- Byte move from array "a" to array "b".  The move must be
 * nondestructive, allowing a byte array to be shifted left or right a
 * few bytes, hence comparison of the addresses of the arrays is necessary
 * to determine if they overlap.
 */
void
BYTMOV (
  XCHAR	*a,			/* input byte array			*/
  XINT	*aoff,			/* first byte in A to be moved		*/
  XCHAR	*b,			/* output byte array			*/
  XINT	*boff,			/* first byte in B to be written	*/
  XINT	*nbytes 		/* number of bytes to move		*/
)
{
	register char	*ip, *op;
	register int	n = *nbytes;
	char	*ap, *bp;

	ap = (char *)a + (*aoff - 1);
	bp = (char *)b + (*boff - 1);

	/* If the two arrays are the same return immediately.  If the move is
	 * to the left then copy left to right, else copy right to left.
	 */
	if (ap == bp) {
	    return;
	} else if (bp < ap) {
	    for (ip=ap, op=bp;  --n >= 0;  )
		*op++ = *ip++;
	} else {
	    for (ip = &ap[n], op = &bp[n];  --n >=  0;  )
		*--op = *--ip;
	}
}
