/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_knames
#define import_xnames
#include <iraf.h>

/*
  IMUPK? -- Convert an array of pixels of datatype DTYPE into the datatype
  specified by the IMUPK? suffix character.
*/

static XINT Error_code = 1;
static XCHAR Error_msg[] = {'U','n','k','n','o','w','n',' ',
			    'd','a','t','a','t','y','p','e',' ','i','n',' ',
			    'i','m','a','g','e','f','i','l','e',XEOS };

int IMUPKR ( void *a, XREAL *b, XSIZE_T *npix, XINT *dtype )
{
	switch ( *dtype ) {
	case TY_USHORT:
	    ACHTUR ((XUSHORT *)a, b, npix);
	    break;
	case TY_SHORT:
	    ACHTSR ((XSHORT *)a, b, npix);
	    break;
	case TY_INT:
	    ACHTIR ((XINT *)a, b, npix);
	    break;
	case TY_LONG:
	    ACHTLR ((XLONG *)a, b, npix);
	    break;
	case TY_REAL:
	    ACHTRR ((XREAL *)a, b, npix);
	    break;
	case TY_DOUBLE:
	    ACHTDR ((XDOUBLE *)a, b, npix);
	    break;
	case TY_COMPLEX:
	    ACHTXR ((XCOMPLEX *)a, b, npix);
	    break;
	default:
	    ERROR (&Error_code, Error_msg);
	    break;
	}

	return 0;
}
