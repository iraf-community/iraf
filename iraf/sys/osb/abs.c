/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_knames
#include <iraf.h>

XINT ABSI ( XINT *a )
{
    if ( *a < 0 ) return 0 - *a;
    else return *a;
}

XLONG ABSL ( XLONG *a )
{
    if ( *a < 0 ) return 0 - *a;
    else return *a;
}

XSHORT ABSS ( XSHORT *a )
{
    if ( *a < 0 ) return 0 - *a;
    else return *a;
}

XPOINTER ABSP ( XPOINTER *a )
{
    if ( *a < 0 ) return 0 - *a;
    else return *a;
}

