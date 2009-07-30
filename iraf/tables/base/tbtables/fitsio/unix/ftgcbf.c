#include <f2c/sys/sys.h>
#include "c_proto.h"
#include "../f2c_proto.h"

int FTGCBF_U(XINT *iunit, XINT *convrt, XINT *nbytes, char *array,
	     XINT *status, ftnlen array_len)
{
    FTGBYT_U(iunit, nbytes, (XCHAR *)array, status);
    ZZEPRO();
    return 0;
}
