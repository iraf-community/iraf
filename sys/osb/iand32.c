/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <string.h>

#define import_spp
#define import_knames
#include <iraf.h>

/* IAND32 - Bitwise AND of long integers.
 */
XINT
IAND32 (XINT *a, XINT *b)
{
    XINT  val = 0;
    int   ia = (int) (*a >> 32), ib = (int) *b;

    val = (ia & ib);
    return ((XINT) val);
}
