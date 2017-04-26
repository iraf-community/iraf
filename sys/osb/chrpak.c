/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define	import_spp
#define import_knames
#include <iraf.h>

/* CHRPAK -- Pack a counted string of XCHAR into bytes.  This routine does not
 * know about EOS terminators.  The input and output arrays may be the same.
 * Note that while XCHAR is signed, the signedness of the C char is unspecified,
 * hence we pack the chars in unsigned bytes, dealing explicitly with any
 * negative values.
 */
CHRPAK (a, a_off, b, b_off, nchars)
XCHAR	*a, *b;
XINT	*a_off, *b_off, *nchars;
{
	register XCHAR	*ip;
	register unsigned char *op;
	register int	n, ch;

	ip = &a[*a_off-1];
	op = &((unsigned char *)b)[*b_off-1];
	n  = *nchars;

	while (--n >= 0)
	    *op++ = ((ch = *ip++) >= 0) ? ch : ch + 256;
}
