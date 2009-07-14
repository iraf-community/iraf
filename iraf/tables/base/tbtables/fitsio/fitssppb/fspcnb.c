#include "c_include.h"

/*
  write an array of unsigned byte data values to the
  specified column of the table.
*/

int FSPCNB_U(XINT *ounit, XINT *colnum, XINT *frow, XINT *felem, XINT *nelem,
	     XINT *array, XINT *nulval, XINT *status)
{
    FTPCNB_U(ounit, colnum, frow, felem, nelem, array, nulval, status,
	     MAX_INT, MAX_INT);
    ZZEPRO();
    return 0;
}
