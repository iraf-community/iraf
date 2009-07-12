#include "c_include.h"

/*
  Read a subsection of byte values from the primary array.
*/

int fsgsfb_(XINT *iunit, XINT *colnum, XINT *naxis, XINT *naxes, XINT *fpixel,
	    XINT *lpixel, XINT *inc, XINT *array, XBOOL *flgval,
	    XBOOL *anyflg, XINT *status)
{
    ftgsfb_(iunit, colnum, naxis, naxes, fpixel, lpixel, inc, array, flgval,
	    anyflg, status, MAX_INT);
    zzepro_();
    return 0;
}
