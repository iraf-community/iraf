#define import_spp
#define import_knames
#include <iraf.h>


/* IMUL32 - Multiply two integer values and return the result.  This is 
 * needed to allow e.g. the normal overflow condition to occur for algorithms
 * such as random number generators.
 */
int
IMUL32 (long *a, long *b)
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
