#include "c_include.h"

/*
  Read a subsection of byte values from the primary array.
*/

int FSGSVB_U(XINT *iunit, XINT *colnum, XINT *naxis, XINT *naxes, XINT *fpixel,
	     XINT *lpixel, XINT *inc, XINT *nulval, XINT *array, XBOOL *anyflg,
	     XINT *status)
{
    FTGSVB_U(iunit, colnum, naxis, naxes, fpixel, lpixel, inc, nulval, array,
	     anyflg, status, MAX_INT, MAX_INT);
    ZZEPRO();
    return 0;
}
