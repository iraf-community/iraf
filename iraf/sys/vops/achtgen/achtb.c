/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_knames
#include <iraf.h>

/*
  ACHT_ -- Convert an array of type _ to some other datatype.
  Data types are BUcsilrdx.
*/

int ACHTB ( XUBYTE *a, void *b, XINT *nelem, XINT *ty_b )
{
	switch ( *ty_b ) {
	case TY_UBYTE:
	    ACHTBB (a, (XUBYTE *)b, nelem);
	    break;
	case TY_USHORT:
	    ACHTBU (a, (XUSHORT *)b, nelem);
	    break;
	case TY_CHAR:
	    ACHTBC (a, (XCHAR *)b, nelem);
	    break;
	case TY_SHORT:
	    ACHTBS (a, (XSHORT *)b, nelem);
	    break;
	case TY_INT:
	    ACHTBI (a, (XINT *)b, nelem);
	    break;
	case TY_POINTER:
	    ACHTBP (a, (XPOINTER *)b, nelem);
	    break;
	case TY_STRUCT:
	    ACHTBP (a, (XPOINTER *)b, nelem);
	    break;
	case TY_LONG:
	    ACHTBL (a, (XLONG *)b, nelem);
	    break;
	case TY_REAL:
	    ACHTBR (a, (XREAL *)b, nelem);
	    break;
	case TY_DOUBLE:
	    ACHTBD (a, (XDOUBLE *)b, nelem);
	    break;
	case TY_COMPLEX:
	    ACHTBX (a, (XCOMPLEX *)b, nelem);
	    break;
	default:
	    break;
	}

	return 0;
}
