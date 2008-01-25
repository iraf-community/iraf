/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_knames
#include <iraf.h>

/* ACHTB_ -- Unpack an unsigned byte array into an SPP array.
 * The loop runs in the reverse direction so that the unpack can be
 * performed in place (a and b can be the same array).
 */
int ACHTBX ( XUBYTE *a, XCOMPLEX *b, XINT *npix )
{
	XUBYTE *ip;
	XCOMPLEX *op;

	for ( ip = a + *npix, op = b + *npix ; b < op ; ) {
	    --op; --ip;
	    op->r = (float) *ip;
	    op->i = 0;
	}

	return 0;
}
