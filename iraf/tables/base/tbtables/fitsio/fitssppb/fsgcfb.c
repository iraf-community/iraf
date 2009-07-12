#include "c_include.h"

/*
  read an array of byte values from a specified column of the table.
  Any undefined pixels will be have the corresponding value of FLGVAL
  set equal to .true., and ANYNUL will be set equal to .true. if
  any pixels are undefined.
*/

int fsgcfb_(XINT *iunit, XINT *colnum, XINT *frow, XINT *felem, XINT *nelem,
	    XINT *array, XBOOL *flgval, XBOOL *anynul, XINT *status)
{
    ftgcfb_(iunit, colnum, frow, felem, nelem, array, flgval, anynul, status,
	    MAX_INT);
    zzepro_();
    return 0;
}
