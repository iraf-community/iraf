#include "c_include.h"

/*
  read an array of byte values from a specified column of the table.
  Any undefined pixels will be set equal to the value of NULVAL,
  unless NULVAL=0, in which case no checks for undefined pixels
  will be made.
*/

int FSGCVB_U(XINT *iunit, XINT *colnum, XLONG *frow, XLONG *felem,
	     XLONG *nelem, XCHAR *nulval, void *array, XBOOL *anynul,
	     XINT *status)
{
    char c_nulval = *nulval;
    XINT i_frow = *frow;
    XINT i_felem = *felem;
    XINT i_nelem = *nelem;
    FTGCVB_U(iunit, colnum, &i_frow, &i_felem, &i_nelem, &c_nulval, array,
	     anynul, status, MAX_INT, MAX_INT);
    *nulval = c_nulval;
    ZZEPRO();
    return 0;
}
