/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_knames
#include <iraf.h>

XSHORT SMAX ( XSHORT *a, XSHORT *b )
{
    return ((*a) >= (*b) ? (*a) : (*b));
}

XLONG LMAX ( XLONG *a, XLONG *b )
{
    return ((*a) >= (*b) ? (*a) : (*b));
}
