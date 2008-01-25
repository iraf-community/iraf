/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_knames
#include <iraf.h>

/*
  ACHT_ -- Convert an array of type _ to some other datatype.
  Data types are BUcsilrdx.
*/

int ACHTU ( XUSHORT *a, void *b, XINT *nelem, XINT *ty_b )
{
	switch ( *ty_b ) {
	case TY_UBYTE:
	    ACHTUB (a, (XUBYTE *)b, nelem);
	    break;
	case TY_USHORT:
	    ACHTUU (a, (XUSHORT *)b, nelem);
	    break;
	case TY_CHAR:
	    ACHTUC (a, (XCHAR *)b, nelem);
	    break;
	case TY_SHORT:
	    ACHTUS (a, (XSHORT *)b, nelem);
	    break;
	case TY_INT:
	    ACHTUI (a, (XINT *)b, nelem);
	    break;
	case TY_POINTER:
	    ACHTUP (a, (XPOINTER *)b, nelem);
	    break;
	case TY_STRUCT:
	    ACHTUP (a, (XPOINTER *)b, nelem);
	    break;
	case TY_LONG:
	    ACHTUL (a, (XLONG *)b, nelem);
	    break;
	case TY_REAL:
	    ACHTUR (a, (XREAL *)b, nelem);
	    break;
	case TY_DOUBLE:
	    ACHTUD (a, (XDOUBLE *)b, nelem);
	    break;
	case TY_COMPLEX:
	    ACHTUX (a, (XCOMPLEX *)b, nelem);
	    break;
	default:
	    break;
	}

	return 0;
}
