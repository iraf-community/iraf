#define import_spp
#define import_knames
#include <iraf.h>

/* ACLRC -- Clear a block of memory.
 * [Specially optimized for Sun/IRAF].
 */
ACLRC (a, n)
XCHAR	*a;
XINT	*n;
{
	bzero ((char *)a, *n * sizeof(*a));
}
