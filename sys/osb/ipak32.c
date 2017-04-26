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
	int  *op = (int  *) calloc (*nelems, sizeof (int));
	int  *tmp, i;

	tmp = op;
	for (i=0; i < *nelems; i++, ip++) {
	    *tmp++ = (int) (*ip);
	}
	memmove (b, op, *nelems * sizeof (int));

	free (op);
}
