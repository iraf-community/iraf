/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_knames
#include <iraf.h>

XINT MODI ( XINT *a, XINT *b )
{
    return (*a) % (*b);
}

XLONG MODL ( XLONG *a, XLONG *b )
{
    return (*a) % (*b);
}

XSHORT MODS ( XSHORT *a, XSHORT *b )
{
    return (*a) % (*b);
}

XPOINTER MODP ( XPOINTER *a, XPOINTER *b )
{
    return (*a) % (*b);
}
