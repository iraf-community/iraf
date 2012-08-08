#define import_spp
#define import_knames
#include <iraf.h>


/* IUPK16 - Unpack 16-bit int into and array of native integers.
 */
void
IUPK16 (void *a, void *b, XINT *nelems)
{
	int    i;
	int   *op = (int *) calloc (*nelems, sizeof (int)), *tmp;
	short *ip = (short *) a;

	tmp = op;
	for (i=0; i < *nelems; i++)
	    *tmp++ = *ip++;

	memmove (b, op, *nelems * sizeof (int));
	free (op);
}
