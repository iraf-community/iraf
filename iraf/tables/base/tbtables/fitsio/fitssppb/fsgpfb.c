#include "c_include.h"

/*
  Read an array of byte values from the primary array.
  Data conversion and scaling will be performed if necessary
  (e.g, if the datatype of the FITS array is not the same
  as the array being read).
  Undefined elements will have the corresponding element of
  FLGVAL set equal to .true.
  ANYNUL is return with a value of .true. if any pixels were undefined.
*/

int fsgpfb_(XINT *iunit, XINT *group, XINT *felem, XINT *nelem, XINT *array,
	    XBOOL *flgval, XBOOL *anynul, XINT *status)
{
    ftgpfb_(iunit, group, felem, nelem, array, flgval, anynul, status,
	    MAX_INT);
    zzepro_();
    return 0;
}
