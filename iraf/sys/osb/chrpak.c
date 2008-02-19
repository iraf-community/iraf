/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_knames
#include <iraf.h>

/* CHRPAK -- Pack a counted string of XCHAR into bytes.  This routine does not
 * know about EOS terminators.  The input and output arrays may be the same.
 * Note that while XCHAR is signed, the signedness of the C char is unspecified,
 * hence we pack the chars in unsigned bytes, dealing explicitly with any
 * negative values.
 */
int CHRPAK ( XCHAR *a, XSIZE_T *a_off, XCHAR *b, XSIZE_T *b_off, XSIZE_T *nchars )
{
	XCHAR *ip;
	unsigned char *op, *maxop;
	int ch;

	ip = &a[*a_off-1];
	op = &((unsigned char *)b)[*b_off-1];
	maxop = op + *nchars -1;

	while ( op <= maxop )
	    *op++ = ((ch = *ip++) >= 0) ? ch : ch + 256;

	return 0;
}
