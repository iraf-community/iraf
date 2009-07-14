#include "c_include.h"

/*
  write an array of unsigned byte data values to the
  specified column of the table.
*/

int FSPCLB_U(XINT *ounit, XINT *colnum, XINT *frow, XINT *felem, XINT *nelem,
	     XINT *array, XINT *status)
{
    FTPCLB_U(ounit, colnum, frow, felem, nelem, array, status, MAX_INT);
    ZZEPRO();
    return 0;
}
