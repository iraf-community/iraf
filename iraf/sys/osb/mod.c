/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_knames
#include <iraf.h>

XINT MODI ( XINT *a, XINT *b )
{
    XINT q = *a / *b;
    return *a - *b * q;
}

XLONG MODL ( XLONG *a, XLONG *b )
{
    XLONG q = *a / *b;
    return *a - *b * q;
}

XSHORT MODS ( XSHORT *a, XSHORT *b )
{
    XSHORT q = *a / *b;
    return *a - *b * q;
}

XPOINTER MODP ( XPOINTER *a, XPOINTER *b )
{
    XPOINTER q = *a / *b;
    return *a - *b * q;
}
