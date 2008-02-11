/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_knames
#include <iraf.h>

/*
  ACHT_ -- Convert an array of type _ to some other datatype.
  Data types are BUcsilrdx.
*/

int ACHTI ( XINT *a, void *b, XSIZE_T *nelem, XINT *ty_b )
{
	switch ( *ty_b ) {
	case TY_UBYTE:
	    ACHTIB (a, (XUBYTE *)b, nelem);
	    break;
	case TY_USHORT:
	    ACHTIU (a, (XUSHORT *)b, nelem);
	    break;
	case TY_CHAR:
	    ACHTIC (a, (XCHAR *)b, nelem);
	    break;
	case TY_SHORT:
	    ACHTIS (a, (XSHORT *)b, nelem);
	    break;
	case TY_INT:
	    ACHTII (a, (XINT *)b, nelem);
	    break;
	case TY_POINTER:
	    ACHTIP (a, (XPOINTER *)b, nelem);
	    break;
	case TY_STRUCT:
	    ACHTIP (a, (XPOINTER *)b, nelem);
	    break;
	case TY_LONG:
	    ACHTIL (a, (XLONG *)b, nelem);
	    break;
	case TY_REAL:
	    ACHTIR (a, (XREAL *)b, nelem);
	    break;
	case TY_DOUBLE:
	    ACHTID (a, (XDOUBLE *)b, nelem);
	    break;
	case TY_COMPLEX:
	    ACHTIX (a, (XCOMPLEX *)b, nelem);
	    break;
	default:
	    break;
	}

	return 0;
}
