/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <math.h>

#define import_spp
#define import_knames
#include <iraf.h>

XLONG NINT_RL ( XREAL *v )
{
    return (XLONG)(0 <= *v ? floor(*v + 0.5) : -floor(0.5 - *v));
}

XLONG NINT_DL ( XDOUBLE *v )
{
    return (XLONG)(0 <= *v ? floor(*v + 0.5) : -floor(0.5 - *v));
}

XINT NINT_RI ( XREAL *v )
{
    return (XINT)(0 <= *v ? floor(*v + 0.5) : -floor(0.5 - *v));
}

XINT NINT_DI ( XDOUBLE *v )
{
    return (XINT)(0 <= *v ? floor(*v + 0.5) : -floor(0.5 - *v));
}

XSHORT NINT_RS ( XREAL *v )
{
    return (XSHORT)(0 <= *v ? floor(*v + 0.5) : -floor(0.5 - *v));
}

XSHORT NINT_DS ( XDOUBLE *v )
{
    return (XSHORT)(0 <= *v ? floor(*v + 0.5) : -floor(0.5 - *v));
}
