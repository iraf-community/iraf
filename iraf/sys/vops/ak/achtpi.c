/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_knames
#include <iraf.h>

/* ACHTP_ -- Pack an SPP datatype array.
 */
int ACHTPI ( XPOINTER *a, XINT *b, XSIZE_T *npix )
{
	XPOINTER *ip;
	XINT *op;

	if ( sizeof(*ip) < sizeof(*op) ) {
	    for ( ip = a + *npix, op = b + *npix ; b < op ; ) {
		--op; --ip;
		*op = *ip;
	    }
	} else {
	    XINT *maxop = b + *npix -1;
	    for ( ip=a, op=b ; op <= maxop ; op++, ip++ ) {
		*op = *ip;
	    }
	}

	return 0;
}
