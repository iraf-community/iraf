/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_knames
#include <iraf.h>

/*
  ACHT_ -- Convert an array of type _ to some other datatype.
  Data types are BUcsilrdx.
*/

int ACHTP ( XPOINTER *a, void *b, XINT *nelem, XINT *ty_b )
{
	switch ( *ty_b ) {
	case TY_UBYTE:
	    ACHTPB (a, (XUBYTE *)b, nelem);
	    break;
	case TY_USHORT:
	    ACHTPU (a, (XUSHORT *)b, nelem);
	    break;
	case TY_CHAR:
	    ACHTPC (a, (XCHAR *)b, nelem);
	    break;
	case TY_SHORT:
	    ACHTPS (a, (XSHORT *)b, nelem);
	    break;
	case TY_INT:
	    ACHTPI (a, (XINT *)b, nelem);
	    break;
	case TY_POINTER:
	    ACHTPP (a, (XPOINTER *)b, nelem);
	    break;
	case TY_STRUCT:
	    ACHTPP (a, (XPOINTER *)b, nelem);
	    break;
	case TY_LONG:
	    ACHTPL (a, (XLONG *)b, nelem);
	    break;
	case TY_REAL:
	    ACHTPR (a, (XREAL *)b, nelem);
	    break;
	case TY_DOUBLE:
	    ACHTPD (a, (XDOUBLE *)b, nelem);
	    break;
	case TY_COMPLEX:
	    ACHTPX (a, (XCOMPLEX *)b, nelem);
	    break;
	default:
	    break;
	}

	return 0;
}
