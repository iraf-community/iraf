/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_knames
#include <iraf.h>

/* BYTMOV -- Byte move from array "a" to array "b".  The move must be
 * nondestructive, allowing a byte array to be shifted left or right a
 * few bytes, hence comparison of the addresses of the arrays is necessary
 * to determine if they overlap.
 */
/* a      : input byte array			*/
/* aoff   : first byte in A to be moved		*/
/* b      : output byte array			*/
/* boff   : first byte in B to be written	*/
/* nbytes : number of bytes to move		*/
int BYTMOV ( void *a, XINT *aoff, void *b, XINT *boff, XINT *nbytes )
{
	long n;
	char *ap, *bp;

	ap = (char *)a + (*aoff - 1);
	bp = (char *)b + (*boff - 1);

	/* If the two arrays are the same return immediately.  If the move is
	 * to the left then copy left to right, else copy right to left.
	 */
	if (ap == bp) {
	    return 0;
	} else if (bp < ap) {
	    for ( n=0 ; n < *nbytes ; n++ )
		bp[n] = ap[n];
	} else {
	    for ( n = *nbytes ; 0 < n ; ) {
		n--;
		bp[n] = ap[n];
	    }
	}

	return 0;
}
