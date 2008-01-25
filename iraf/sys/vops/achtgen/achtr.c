/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_knames
#include <iraf.h>

/*
  ACHT_ -- Convert an array of type _ to some other datatype.
  Data types are BUcsilrdx.
*/

int ACHTR ( XREAL *a, void *b, XINT *nelem, XINT *ty_b )
{
	switch ( *ty_b ) {
	case TY_UBYTE:
	    ACHTRB (a, (XUBYTE *)b, nelem);
	    break;
	case TY_USHORT:
	    ACHTRU (a, (XUSHORT *)b, nelem);
	    break;
	case TY_CHAR:
	    ACHTRC (a, (XCHAR *)b, nelem);
	    break;
	case TY_SHORT:
	    ACHTRS (a, (XSHORT *)b, nelem);
	    break;
	case TY_INT:
	    ACHTRI (a, (XINT *)b, nelem);
	    break;
	case TY_POINTER:
	    ACHTRP (a, (XPOINTER *)b, nelem);
	    break;
	case TY_STRUCT:
	    ACHTRP (a, (XPOINTER *)b, nelem);
	    break;
	case TY_LONG:
	    ACHTRL (a, (XLONG *)b, nelem);
	    break;
	case TY_REAL:
	    ACHTRR (a, (XREAL *)b, nelem);
	    break;
	case TY_DOUBLE:
	    ACHTRD (a, (XDOUBLE *)b, nelem);
	    break;
	case TY_COMPLEX:
	    ACHTRX (a, (XCOMPLEX *)b, nelem);
	    break;
	default:
	    break;
	}

	return 0;
}
