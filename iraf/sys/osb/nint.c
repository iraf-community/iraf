/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <math.h>

#define import_spp
#define import_knames
#include <iraf.h>

/*
 * snint(), sdnint(), inint(), lnint() and ldnint()
 */

XSHORT SNINT ( XREAL *v )
{
    return (XSHORT)(0 <= *v ? (*v + 0.5) : (*v - 0.5));
}

XSHORT SDNINT ( XDOUBLE *v )
{
    return (XSHORT)(0 <= *v ? (*v + 0.5) : (*v - 0.5));
}

XINT ININT ( XREAL *v )
{
    return (XINT)(0 <= *v ? (*v + 0.5) : (*v - 0.5));
}

/* IDNINT() is supported by F77 */

XLONG LNINT ( XREAL *v )
{
    return (XLONG)(0 <= *v ? (*v + 0.5) : (*v - 0.5));
}

XLONG LDNINT ( XDOUBLE *v )
{
    return (XLONG)(0 <= *v ? (*v + 0.5) : (*v - 0.5));
}

/*
 * sint(), sdint(), iint(), lint() and ldint()
 */

XSHORT SINT ( XREAL *v )
{
    return (XSHORT)(*v);
}

XSHORT SDINT ( XDOUBLE *v )
{
    return (XSHORT)(*v);
}

XINT IINT ( XREAL *v )
{
    return (XINT)(*v);
}

/* IDINT() is supported by F77 */

XLONG LINT ( XREAL *v )
{
    return (XLONG)(*v);
}

XLONG LDINT ( XDOUBLE *v )
{
    return (XLONG)(*v);
}

