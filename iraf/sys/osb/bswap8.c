/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_knames
#include <iraf.h>

/* BSWAP8 - Move bytes from array "a" to array "b", swapping the eight bytes
 * in each successive 8 byte group, i.e., 12345678 becomes 87654321.
 * The input and output arrays may be the same but may not partially overlap.
 */
/* a      : input array			*/
/* aoff   : first byte in input array	*/
/* b      : output array		*/
/* boff   : first byte in output array	*/
/* nbytes : number of bytes to swap	*/
int BSWAP8 ( XCHAR *a, XINT *aoff, XCHAR *b, XINT *boff, XINT *nbytes )
{
	static char temp[8];
	char *ip, *op, *tp;
	long n;

	tp = temp;
	ip = (char *)a + *aoff - 1;
	op = (char *)b + *boff - 1;

	/* Swap successive eight byte groups.
	 */
	for ( n = *nbytes >> 3 ; 0 < n ; n-- ) {
	    *tp++ = *ip++;
	    *tp++ = *ip++;
	    *tp++ = *ip++;
	    *tp++ = *ip++;
	    *tp++ = *ip++;
	    *tp++ = *ip++;
	    *tp++ = *ip++;
	    *tp++ = *ip++;
	    *op++ = *--tp;
	    *op++ = *--tp;
	    *op++ = *--tp;
	    *op++ = *--tp;
	    *op++ = *--tp;
	    *op++ = *--tp;
	    *op++ = *--tp;
	    *op++ = *--tp;
	}

	/* If there are any odd bytes left, move them to the output array.
	 * Do not bother to swap as it is unclear how to swap a partial
	 * group, and really incorrect if the data is not modulus 8.
	 */
	for ( n = *nbytes & 07 ; 0 < n ; n-- )
	    *op++ = *ip++;

	return 0;
}
