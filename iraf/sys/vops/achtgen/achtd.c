/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_knames
#include <iraf.h>

/*
  ACHT_ -- Convert an array of type _ to some other datatype.
  Data types are BUcsilrdx.
*/

int ACHTD ( XDOUBLE *a, void *b, XINT *nelem, XINT *ty_b )
{
	switch ( *ty_b ) {
	case TY_UBYTE:
	    ACHTDB (a, (XUBYTE *)b, nelem);
	    break;
	case TY_USHORT:
	    ACHTDU (a, (XUSHORT *)b, nelem);
	    break;
	case TY_CHAR:
	    ACHTDC (a, (XCHAR *)b, nelem);
	    break;
	case TY_SHORT:
	    ACHTDS (a, (XSHORT *)b, nelem);
	    break;
	case TY_INT:
	    ACHTDI (a, (XINT *)b, nelem);
	    break;
	case TY_POINTER:
	    ACHTDP (a, (XPOINTER *)b, nelem);
	    break;
	case TY_STRUCT:
	    ACHTDP (a, (XPOINTER *)b, nelem);
	    break;
	case TY_LONG:
	    ACHTDL (a, (XLONG *)b, nelem);
	    break;
	case TY_REAL:
	    ACHTDR (a, (XREAL *)b, nelem);
	    break;
	case TY_DOUBLE:
	    ACHTDD (a, (XDOUBLE *)b, nelem);
	    break;
	case TY_COMPLEX:
	    ACHTDX (a, (XCOMPLEX *)b, nelem);
	    break;
	default:
	    break;
	}

	return 0;
}
