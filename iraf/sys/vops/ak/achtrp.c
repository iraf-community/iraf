/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_knames
#include <iraf.h>

/* ACHT_P -- Pack an SPP datatype array.
 */
int ACHTRP ( XREAL *a, XPOINTER *b, XINT *npix )
{
	XREAL *ip;
	XPOINTER *op;

	if ( sizeof(*ip) < sizeof(*op) ) {
	    for ( ip = a + *npix, op = b + *npix ; b < op ; ) {
		--op; --ip;
		*op = *ip;
	    }
	} else {
	    XPOINTER *maxop = b + *npix -1;
	    for ( ip=a, op=b ; op <= maxop ; op++, ip++ ) {
		*op = *ip;
	    }
	}

	return 0;
}
