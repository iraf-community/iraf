/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_knames
#include <iraf.h>

/* ACHTBB -- Unpack an unsigned byte array into an SPP array.
 */
int ACHTBB ( XUBYTE *a, XUBYTE *b, XINT *npix )
{
	XUBYTE *ip;
	XUBYTE *op;

	if ( a < b ) {
	    for ( ip = a + *npix, op = b + *npix ; b < op ; ) {
		--op; --ip;
		*op = *ip;
	    }
	}
	else {
	    XUBYTE *maxop = b + *npix -1;
	    for ( ip = a, op = b ; op <= maxop ; ip++, op++ ) {
		*op = *ip;
	    }
	}

	return 0;
}
