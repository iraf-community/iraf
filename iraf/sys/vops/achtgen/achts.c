/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_knames
#include <iraf.h>

/*
  ACHT_ -- Convert an array of type _ to some other datatype.
  Data types are BUcsilrdx.
*/

int ACHTS ( XSHORT *a, void *b, XINT *nelem, XINT *ty_b )
{
	switch ( *ty_b ) {
	case TY_UBYTE:
	    ACHTSB (a, (XUBYTE *)b, nelem);
	    break;
	case TY_USHORT:
	    ACHTSU (a, (XUSHORT *)b, nelem);
	    break;
	case TY_CHAR:
	    ACHTSC (a, (XCHAR *)b, nelem);
	    break;
	case TY_SHORT:
	    ACHTSS (a, (XSHORT *)b, nelem);
	    break;
	case TY_INT:
	    ACHTSI (a, (XINT *)b, nelem);
	    break;
	case TY_POINTER:
	    ACHTSP (a, (XPOINTER *)b, nelem);
	    break;
	case TY_STRUCT:
	    ACHTSP (a, (XPOINTER *)b, nelem);
	    break;
	case TY_LONG:
	    ACHTSL (a, (XLONG *)b, nelem);
	    break;
	case TY_REAL:
	    ACHTSR (a, (XREAL *)b, nelem);
	    break;
	case TY_DOUBLE:
	    ACHTSD (a, (XDOUBLE *)b, nelem);
	    break;
	case TY_COMPLEX:
	    ACHTSX (a, (XCOMPLEX *)b, nelem);
	    break;
	default:
	    break;
	}

	return 0;
}
