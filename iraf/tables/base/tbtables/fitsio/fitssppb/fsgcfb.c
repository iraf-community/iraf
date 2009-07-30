#include "c_include.h"

/*
  read an array of byte values from a specified column of the table.
  Any undefined pixels will be have the corresponding value of FLGVAL
  set equal to .true., and ANYNUL will be set equal to .true. if
  any pixels are undefined.
*/

int FSGCFB_U(XINT *iunit, XINT *colnum, XINT *frow, XINT *felem, XINT *nelem,
	     void *array, XBOOL *flgval, XBOOL *anynul, XINT *status)
{
    FTGCFB_U(iunit, colnum, frow, felem, nelem, array, flgval, anynul, status,
	     MAX_INT);
    ZZEPRO();
    return 0;
}
