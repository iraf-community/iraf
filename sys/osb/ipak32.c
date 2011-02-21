#define import_spp
#define import_knames
#include <iraf.h>

/* IPAK32 - Pack 64-bit int into and array of 32-bit int.
 */
void
IPAK32 (void *a, void *b, XINT *nelems)
{
	/* MACHDEP - Works only for little-endian systems (e.g. x86)
	*/
	XINT *ip = (XINT *) a;
	int  *op = (int  *) b;
	int   i = 0;

	for (i=0; i < *nelems; i++) {
	    *op = (int) *ip;
	    op++, ip++;
	}
}
