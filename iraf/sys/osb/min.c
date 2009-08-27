/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_knames
#include <iraf.h>

XSHORT SMIN ( XSHORT *a, XSHORT *b )
{
    return ((*a) <= (*b) ? (*a) : (*b));
}

XLONG LMIN ( XLONG *a, XLONG *b )
{
    return ((*a) <= (*b) ? (*a) : (*b));
}
