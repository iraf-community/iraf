#include "c_include.h"

/*
  Read a subsection of byte values from the primary array.
*/

int FSGSVB_U(XINT *iunit, XINT *colnum, XINT *naxis, XINT *naxes, XINT *fpixel,
	     XINT *lpixel, XINT *inc, XCHAR *nulval, void *array,
	     XBOOL *anyflg, XINT *status)
{
    char c_nulval = *nulval;
    FTGSVB_U(iunit, colnum, naxis, naxes, fpixel, lpixel, inc, &c_nulval,
	     array, anyflg, status, MAX_INT, MAX_INT);
    *nulval = c_nulval;
    ZZEPRO();
    return 0;
}
