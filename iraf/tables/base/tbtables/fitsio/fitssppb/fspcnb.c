#include "c_include.h"

/*
  write an array of unsigned byte data values to the
  specified column of the table.
*/

int fspcnb_(XINT *ounit, XINT *colnum, XINT *frow, XINT *felem, XINT *nelem,
	    XINT *array, XINT *nulval, XINT *status)
{
    ftpcnb_(ounit, colnum, frow, felem, nelem, array, nulval, status,
	    MAX_INT, MAX_INT);
    zzepro_();
    return 0;
}
