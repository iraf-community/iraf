/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_knames
#include <iraf.h>

/* ACHTUU -- Unpack an unsigned short integer array into an SPP datatype.
 */
int ACHTUU ( XUSHORT *a, XUSHORT *b, XINT *npix )
{
	XUSHORT *ip;
	XUSHORT *op;

	if ( a < b ) {
	    for ( ip = a + *npix, op = b + *npix ; b < op ; ) {
		--op; --ip;
		*op = *ip;
	    }
	}
	else {
	    XUSHORT *maxop = b + *npix -1;
	    for ( ip = a, op = b ; op <= maxop ; ip++, op++ ) {
		*op = *ip;
	    }
	}

	return 0;
}
