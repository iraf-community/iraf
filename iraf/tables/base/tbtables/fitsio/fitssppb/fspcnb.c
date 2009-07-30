#include "c_include.h"

/*
  write an array of unsigned byte data values to the
  specified column of the table.
*/

int FSPCNB_U(XINT *ounit, XINT *colnum, XINT *frow, XINT *felem, XINT *nelem,
	     void *array, XCHAR *nulval, XINT *status)
{
    char c_nulval = *nulval;
    FTPCNB_U(ounit, colnum, frow, felem, nelem, array, &c_nulval, status,
	     MAX_INT, MAX_INT);
    *nulval = c_nulval;
    ZZEPRO();
    return 0;
}
