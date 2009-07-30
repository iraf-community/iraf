#include "c_include.h"

/*
  Read an array of byte values from the primary array.
  Data conversion and scaling will be performed if necessary
  (e.g, if the datatype of the FITS array is not the same
  as the array being read).
  Undefined elements will be set equal to NULVAL, unless NULVAL=0
  in which case no checking for undefined values will be performed.
  ANYNUL is return with a value of .true. if any pixels were undefined.
*/

int FSGPVB_U(XINT *iunit, XINT *group, XINT *felem, XINT *nelem, XCHAR *nulval,
	     void *array, XBOOL *anynul, XINT *status)
{
    char c_nulval = *nulval;
    FTGPVB_U(iunit, group, felem, nelem, &c_nulval, array, anynul, status,
	     MAX_INT, MAX_INT);
    *nulval = c_nulval;
    ZZEPRO();
    return 0;
}
