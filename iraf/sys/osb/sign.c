/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_knames
#include <iraf.h>

XSHORT SSIGN ( XSHORT *a, XSHORT *b )
{
    XSHORT x;
    x = (*a >= 0 ? *a : - *a);
    return (*b >= 0 ? x : -x);
}

XLONG LSIGN ( XLONG *a, XLONG *b )
{
    XLONG x;
    x = (*a >= 0 ? *a : - *a);
    return (*b >= 0 ? x : -x);
}

XREAL ASIGN ( XREAL *a, XREAL *b )
{
    XREAL x;
    x = (*a >= 0 ? *a : - *a);
    return (*b >= 0 ? x : -x);
}
