/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define	import_spp
#define import_knames
#include <iraf.h>

/* CHRUPK -- Unpack a byte string into XCHAR.  This routine does not
 * know about EOS terminators.  The input and output arrays may be the same.
 * Note that while XCHAR is signed, the signedness of the C char is unspecified,
 * hence we pack the chars into unsigned bytes and restore the sign explicitly.
 */
CHRUPK (a, a_off, b, b_off, nchars)
XCHAR	*a, *b;
XINT	*a_off, *b_off, *nchars;
{
	register unsigned char *ip;
	register XCHAR	*op;
	register int	n, ch;

	/* Set pointers to last char plus one so that we can unpack the array
	 * in the reverse direction.
	 */
	n = *nchars;
	ip = &((unsigned char *)a)[*a_off-1+n];
	op = &b[*b_off-1+n];

	/* Unpack string from right to left.
	 */
	while (--n >= 0)
	    *--op = ((ch = *--ip) <= 127) ? ch : ch - 256;
}
