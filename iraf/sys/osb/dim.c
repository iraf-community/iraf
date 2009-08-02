/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_knames
#include <iraf.h>

XSHORT SDIM ( XSHORT *a, XSHORT *b )
{
    return (*a > *b ? *a - *b : 0);
}

XLONG LDIM ( XLONG *a, XLONG *b )
{
    return (*a > *b ? *a - *b : 0);
}

XREAL ADIM ( XREAL *a, XREAL *b )
{
    return (*a > *b ? *a - *b : 0);
}
