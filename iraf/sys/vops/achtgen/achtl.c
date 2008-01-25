/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_knames
#include <iraf.h>

/*
  ACHT_ -- Convert an array of type _ to some other datatype.
  Data types are BUcsilrdx.
*/

int ACHTL ( XLONG *a, void *b, XINT *nelem, XINT *ty_b )
{
	switch ( *ty_b ) {
	case TY_UBYTE:
	    ACHTLB (a, (XUBYTE *)b, nelem);
	    break;
	case TY_USHORT:
	    ACHTLU (a, (XUSHORT *)b, nelem);
	    break;
	case TY_CHAR:
	    ACHTLC (a, (XCHAR *)b, nelem);
	    break;
	case TY_SHORT:
	    ACHTLS (a, (XSHORT *)b, nelem);
	    break;
	case TY_INT:
	    ACHTLI (a, (XINT *)b, nelem);
	    break;
	case TY_POINTER:
	    ACHTLP (a, (XPOINTER *)b, nelem);
	    break;
	case TY_STRUCT:
	    ACHTLP (a, (XPOINTER *)b, nelem);
	    break;
	case TY_LONG:
	    ACHTLL (a, (XLONG *)b, nelem);
	    break;
	case TY_REAL:
	    ACHTLR (a, (XREAL *)b, nelem);
	    break;
	case TY_DOUBLE:
	    ACHTLD (a, (XDOUBLE *)b, nelem);
	    break;
	case TY_COMPLEX:
	    ACHTLX (a, (XCOMPLEX *)b, nelem);
	    break;
	default:
	    break;
	}

	return 0;
}
