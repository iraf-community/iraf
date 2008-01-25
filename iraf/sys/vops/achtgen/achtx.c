/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_knames
#include <iraf.h>

/*
  ACHT_ -- Convert an array of type _ to some other datatype.
  Data types are BUcsilrdx.
*/

int ACHTX ( XCOMPLEX *a, void *b, XINT *nelem, XINT *ty_b )
{
	switch ( *ty_b ) {
	case TY_UBYTE:
	    ACHTXB (a, (XUBYTE *)b, nelem);
	    break;
	case TY_USHORT:
	    ACHTXU (a, (XUSHORT *)b, nelem);
	    break;
	case TY_CHAR:
	    ACHTXC (a, (XCHAR *)b, nelem);
	    break;
	case TY_SHORT:
	    ACHTXS (a, (XSHORT *)b, nelem);
	    break;
	case TY_INT:
	    ACHTXI (a, (XINT *)b, nelem);
	    break;
	case TY_POINTER:
	    ACHTXP (a, (XPOINTER *)b, nelem);
	    break;
	case TY_STRUCT:
	    ACHTXP (a, (XPOINTER *)b, nelem);
	    break;
	case TY_LONG:
	    ACHTXL (a, (XLONG *)b, nelem);
	    break;
	case TY_REAL:
	    ACHTXR (a, (XREAL *)b, nelem);
	    break;
	case TY_DOUBLE:
	    ACHTXD (a, (XDOUBLE *)b, nelem);
	    break;
	case TY_COMPLEX:
	    ACHTXX (a, (XCOMPLEX *)b, nelem);
	    break;
	default:
	    break;
	}

	return 0;
}
