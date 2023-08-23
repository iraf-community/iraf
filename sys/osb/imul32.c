/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_knames
#include <iraf.h>


/* IMUL32 - Multiply two integer values and return the result.  This is 
 * needed to allow e.g. the normal overflow condition to occur for algorithms
 * such as random number generators.
 */
int
IMUL32 (XINT *a, XINT *b)
{
    int val = 0;
    int  ia = (int) *a;
    int  ib = (int) *b;


    /*  MACHDEP - Depends on integer overflow behavior for a specific
     *  platform.
     */
    val = ia * ib;

    return ((int) val);
}
