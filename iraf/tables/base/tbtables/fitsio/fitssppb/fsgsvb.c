#include "c_include.h"

/*
  Read a subsection of byte values from the primary array.
*/

int fsgsvb_(XINT *iunit, XINT *colnum, XINT *naxis, XINT *naxes, XINT *fpixel,
	    XINT *lpixel, XINT *inc, XINT *nulval, XINT *array, XBOOL *anyflg,
	    XINT *status)
{
    ftgsvb_(iunit, colnum, naxis, naxes, fpixel, lpixel, inc, nulval, array,
	    anyflg, status, MAX_INT, MAX_INT);
    zzepro_();
    return 0;
}
