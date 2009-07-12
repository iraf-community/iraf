#include "c_include.h"

/*
  write an array of unsigned byte data values to the
  specified column of the table.
*/

int fspclb_(XINT *ounit, XINT *colnum, XINT *frow, XINT *felem, XINT *nelem,
	    XINT *array, XINT *status)
{
    ftpclb_(ounit, colnum, frow, felem, nelem, array, status, MAX_INT);
    zzepro_();
    return 0;
}
