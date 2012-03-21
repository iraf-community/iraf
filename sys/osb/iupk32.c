#define import_spp
#define import_knames
#include <iraf.h>


/* IUPK32 - Unpack 32-bit int into and array of 64-bit int.
 */
void
IUPK32 (void *a, void *b, XINT *nelems)
{
	XINT i, *tmp;
	XINT *op = (XINT *) calloc (*nelems, sizeof (XINT));
	int  *ip = (int *) a;


	tmp = op;
	for (i=0; i < *nelems; i++) {
	    *tmp++ = *ip++;
	}
	memmove (b, op, *nelems * sizeof (XINT));

	free (op);
}
