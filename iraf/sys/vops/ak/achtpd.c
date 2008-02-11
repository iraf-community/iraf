/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_knames
#include <iraf.h>

/* ACHTP_ -- Pack an SPP datatype array.
 */
int ACHTPD ( XPOINTER *a, XDOUBLE *b, XSIZE_T *npix )
{
	XPOINTER *ip;
	XDOUBLE *op;

	if ( sizeof(*ip) < sizeof(*op) ) {
	    for ( ip = a + *npix, op = b + *npix ; b < op ; ) {
		--op; --ip;
		*op = *ip;
	    }
	} else {
	    XDOUBLE *maxop = b + *npix -1;
	    for ( ip=a, op=b ; op <= maxop ; op++, ip++ ) {
		*op = *ip;
	    }
	}

	return 0;
}
