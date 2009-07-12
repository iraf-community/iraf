#include "c_include.h"

/*
  read an array of byte values from a specified column of the table.
  Any undefined pixels will be set equal to the value of NULVAL,
  unless NULVAL=0, in which case no checks for undefined pixels
  will be made.
*/

int fsgcvb_(XINT *iunit, XINT *colnum, XINT *frow, XINT *felem, XINT *nelem,
	    XINT *nulval, XINT *array, XBOOL *anynul, XINT *status)
{
    ftgcvb_(iunit, colnum, frow, felem, nelem, nulval, array, anynul, status,
	    MAX_INT, MAX_INT);
    zzepro_();
    return 0;
}
