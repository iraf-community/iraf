#include "c_include.h"

/*
  Binary table data DEFinition
  define the structure of the binary table data unit
*/

#define TFORM_LEN 16
#define MAX_NFIELD 512

int FSBDEF_U(XINT *ounit, XINT *nfield, XSHORT *tform, XINT *pcount,
	     XINT *nrows, XINT *status)
{
    int i;
    static char ftform[TFORM_LEN*MAX_NFIELD];
    static XINT c_70 = 70;

    for ( i = 0 ; i < *nfield && i < MAX_NFIELD ; i++ ) {
	F77PAK(&tform[i * 71], ftform + (i * TFORM_LEN), &c_70, TFORM_LEN);
    }
    FTBDEF_U(ounit, nfield, &ftform, pcount, nrows, status,
	     TFORM_LEN * MAX_NFIELD);

    ZZEPRO();
    return 0;
}
