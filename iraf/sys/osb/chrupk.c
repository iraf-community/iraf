/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_knames
#include <iraf.h>

/* CHRUPK -- Unpack a byte string into XCHAR.  This routine does not
 * know about EOS terminators.  The input and output arrays may be the same.
 * Note that while XCHAR is signed, the signedness of the C char is unspecified,
 * hence we pack the chars into unsigned bytes and restore the sign explicitly.
 */
int CHRUPK ( XCHAR *a, XINT *a_off, XCHAR *b, XINT *b_off, XINT *nchars )
{
	unsigned char *ip;
	XCHAR *op, *minop;
	int ch;

	/* Set pointers to last char plus one so that we can unpack the array
	 * in the reverse direction.
	 */
	ip = &((unsigned char *)a)[*a_off - 1 + *nchars];
	op = &b[*b_off - 1 + *nchars];
	minop = op - *nchars + 1;

	/* Unpack string from right to left.
	 */
	while ( minop <= op )
	    *--op = ((ch = *--ip) <= 127) ? ch : ch - 256;

	return 0;
}
