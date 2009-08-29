/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_knames
#include <iraf.h>

XSHORT SMOD ( XSHORT *a, XSHORT *b )
{
    return (*a) % (*b);
}

XINT IMOD ( XINT *a, XINT *b )
{
    return (*a) % (*b);
}

XLONG LMOD ( XLONG *a, XLONG *b )
{
    return (*a) % (*b);
}

/*
XPOINTER PMOD ( XPOINTER *a, XPOINTER *b )
{
    return (*a) % (*b);
}
*/
