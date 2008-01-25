/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_knames
#include <iraf.h>

/*
  ACHT -- General data type conversion based on the generic routines
  The data types are BUcsilrdx.
*/

int ACHT ( void *a, void *b, XINT *nelem, XINT *ty_a, XINT *ty_b )
{
 	switch ( *ty_a ) {
	case TY_UBYTE:
	    ACHTB ((XUBYTE *)a, b, nelem, ty_b);
	    break;
	case TY_USHORT:
	    ACHTU ((XUSHORT *)a, b, nelem, ty_b);
	    break;
	case TY_CHAR:
	    ACHTC ((XCHAR *)a, b, nelem, ty_b);
	    break;
	case TY_SHORT:
	    ACHTS ((XSHORT *)a, b, nelem, ty_b);
	    break;
	case TY_INT:
	    ACHTI ((XINT *)a, b, nelem, ty_b);
	    break;
	case TY_POINTER:
	    ACHTP ((XPOINTER *)a, b, nelem, ty_b);
	    break;
	case TY_STRUCT:
	    ACHTP ((XPOINTER *)a, b, nelem, ty_b);
	    break;
	case TY_LONG:
	    ACHTL ((XLONG *)a, b, nelem, ty_b);
	    break;
	case TY_REAL:
	    ACHTR ((XREAL *)a, b, nelem, ty_b);
	    break;
	case TY_DOUBLE:
	    ACHTD ((XDOUBLE *)a, b, nelem, ty_b);
	    break;
	case TY_COMPLEX:
	    ACHTX ((XCOMPLEX *)a, b, nelem, ty_b);
	    break;
	default:
	    break;
	}

	return 0;
}
