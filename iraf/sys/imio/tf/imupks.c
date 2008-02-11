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

int IMUPKS ( void *a, XSHORT *b, XSIZE_T *npix, XINT *dtype )
{
	switch ( *dtype ) {
	case TY_USHORT:
	    ACHTUS ((XUSHORT *)a, b, npix);
	    break;
	case TY_SHORT:
	    ACHTSS ((XSHORT *)a, b, npix);
	    break;
	case TY_INT:
	    ACHTIS ((XINT *)a, b, npix);
	    break;
	case TY_LONG:
	    ACHTLS ((XLONG *)a, b, npix);
	    break;
	case TY_REAL:
	    ACHTRS ((XREAL *)a, b, npix);
	    break;
	case TY_DOUBLE:
	    ACHTDS ((XDOUBLE *)a, b, npix);
	    break;
	case TY_COMPLEX:
	    ACHTXS ((XCOMPLEX *)a, b, npix);
	    break;
	default:
	    ERROR (&Error_code, Error_msg);
	    break;
	}

	return 0;
}
