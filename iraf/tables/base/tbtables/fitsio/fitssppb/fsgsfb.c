#include "c_include.h"

/*
  Read a subsection of byte values from the primary array.
*/

int FSGSFB_U(XINT *iunit, XINT *colnum, XINT *naxis, XINT *naxes, XINT *fpixel,
	     XINT *lpixel, XINT *inc, XINT *array, XBOOL *flgval,
	     XBOOL *anyflg, XINT *status)
{
    FTGSFB_U(iunit, colnum, naxis, naxes, fpixel, lpixel, inc, array, flgval,
	     anyflg, status, MAX_INT);
    ZZEPRO();
    return 0;
}
