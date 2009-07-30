#include "c_include.h"

/*
  write an array of unsigned byte data values to the
  specified column of the table.
*/

int FSPCLB_U(XINT *ounit, XINT *colnum, XLONG *frow, XLONG *felem,
	     XLONG *nelem, void *array, XINT *status)
{
    XINT i_frow = *frow;
    XINT i_felem = *felem;
    XINT i_nelem = *nelem;

    FTPCLB_U(ounit, colnum, &i_frow, &i_felem, &i_nelem, array, status,
	     MAX_INT);
    ZZEPRO();
    return 0;
}
