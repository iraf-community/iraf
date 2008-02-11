/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_knames
#include <iraf.h>

/* ACHT_U -- Pack an SPP datatype array into an unsigned short integer.
 * [MACHDEP]: The underscore appended to the procedure name is OS dependent.
 */
int ACHTSU ( XSHORT *a, XUSHORT *b, XSIZE_T *npix )
{
	XSHORT *ip;
	XUSHORT *op;

	if ( sizeof(*ip) < sizeof(*op) ) {
	    for ( ip = a + *npix, op = b + *npix ; b < op ; ) {
		--op; --ip;
		*op = *ip;
	    }
	} else {
	    XUSHORT *maxop = b + *npix -1;
	    for ( ip=a, op=b ; op <= maxop ; op++, ip++ ) {
		*op = *ip;
	    }
	}

	return 0;
}
