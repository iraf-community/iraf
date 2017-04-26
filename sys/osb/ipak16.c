#define import_spp
#define import_knames
#include <iraf.h>

/* IPAK16 - Pack an array of native ints into and array of 16-bit short.
 */
void
IPAK16 (void *a, void *b, XINT *nelems)
{
	/* MACHDEP - Works only for little-endian systems (e.g. x86)
	*/
	int    i  = 0;
	int   *ip = (int *) a;
	short *op = (short  *) b;

	for (i=0; i < *nelems; i++) {
	    *op = (int) *ip;
	    op++, ip++;
	}
}
