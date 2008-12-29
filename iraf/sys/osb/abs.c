/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_knames
#include <iraf.h>

XSHORT SABS ( XSHORT *a )
{
    if ( *a < 0 ) return 0 - *a;
    else return *a;
}

XLONG LABS ( XLONG *a )
{
    if ( *a < 0 ) return 0 - *a;
    else return *a;
}

XPOINTER PABS ( XPOINTER *a )
{
    if ( *a < 0 ) return 0 - *a;
    else return *a;
}

XREAL AABS ( XREAL *a )
{
    if ( *a < 0 ) return 0 - *a;
    else return *a;
}
