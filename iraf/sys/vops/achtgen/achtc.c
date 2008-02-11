/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_knames
#include <iraf.h>

/*
  ACHT_ -- Convert an array of type _ to some other datatype.
  Data types are BUcsilrdx.
*/

int ACHTC ( XCHAR *a, void *b, XSIZE_T *nelem, XINT *ty_b )
{
	switch ( *ty_b ) {
	case TY_UBYTE:
	    ACHTCB (a, (XUBYTE *)b, nelem);
	    break;
	case TY_USHORT:
	    ACHTCU (a, (XUSHORT *)b, nelem);
	    break;
	case TY_CHAR:
	    ACHTCC (a, (XCHAR *)b, nelem);
	    break;
	case TY_SHORT:
	    ACHTCS (a, (XSHORT *)b, nelem);
	    break;
	case TY_INT:
	    ACHTCI (a, (XINT *)b, nelem);
	    break;
	case TY_POINTER:
	    ACHTCP (a, (XPOINTER *)b, nelem);
	    break;
	case TY_STRUCT:
	    ACHTCP (a, (XPOINTER *)b, nelem);
	    break;
	case TY_LONG:
	    ACHTCL (a, (XLONG *)b, nelem);
	    break;
	case TY_REAL:
	    ACHTCR (a, (XREAL *)b, nelem);
	    break;
	case TY_DOUBLE:
	    ACHTCD (a, (XDOUBLE *)b, nelem);
	    break;
	case TY_COMPLEX:
	    ACHTCX (a, (XCOMPLEX *)b, nelem);
	    break;
	default:
	    break;
	}

	return 0;
}
