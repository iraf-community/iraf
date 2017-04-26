/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define	import_spp
#define import_knames
#include <iraf.h>

/* BSWAP2 - Move bytes from array "a" to array "b", swapping successive
 * pairs of bytes.  The two arrays may be the same but may not be offset
 * and overlapping.
 */
BSWAP2 (a, aoff, b, boff, nbytes)
XCHAR	*a;			/* input array			*/
XINT	*aoff;			/* first byte in input array	*/
XCHAR	*b;			/* output array			*/
XINT	*boff;			/* first byte in output array	*/
XINT	*nbytes;		/* number of bytes to swap	*/
{
	register char *ip, *op, *otop;
	register unsigned temp;

	ip = (char *)a + *aoff - 1;
	op = (char *)b + *boff - 1;
	otop = op + (*nbytes & ~1);

	/* Swap successive pairs of bytes.
	 */
	while (op < otop) {
	    temp  = *ip++;
	    *op++ = *ip++;
	    *op++ = temp;
	}

	/* If there is an odd byte left, move it to the output array.
	 */
	if (*nbytes & 1)
	    *op = *ip;
}
