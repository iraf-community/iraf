/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define	import_spp
#define import_knames
#include <iraf.h>

/* BSWAP4 - Move bytes from array "a" to array "b", swapping the four bytes
 * in each successive 4 byte group, i.e., 12345678 becomes 43218765.
 * The input and output arrays may be the same but may not partially overlap.
 */
BSWAP4 (a, aoff, b, boff, nbytes)
XCHAR	*a;			/* input array			*/
XINT	*aoff;			/* first byte in input array	*/
XCHAR	*b;			/* output array			*/
XINT	*boff;			/* first byte in output array	*/
XINT	*nbytes;		/* number of bytes to swap	*/
{
	register char	*ip, *op, *tp;
	register int	n;
	static	char temp[4];

	tp = temp;
	ip = (char *)a + *aoff - 1;
	op = (char *)b + *boff - 1;

	/* Swap successive four byte groups.
	 */
	for (n = *nbytes >> 2;  --n >= 0;  ) {
	    *tp++ = *ip++;
	    *tp++ = *ip++;
	    *tp++ = *ip++;
	    *tp++ = *ip++;
	    *op++ = *--tp;
	    *op++ = *--tp;
	    *op++ = *--tp;
	    *op++ = *--tp;
	}

	/* If there are any odd bytes left, move them to the output array.
	 * Do not bother to swap as it is unclear how to swap a partial
	 * group, and really incorrect if the data is not modulus 4.
	 */
	for (n = *nbytes & 03;  --n >= 0;  )
	    *op++ = *ip++;
}
