/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_knames
#include <iraf.h>

/* ACLRB -- Clear a block of memory.
 */
int ACLRB ( void *a, XINT *nbytes )
{
	char *p;
	char *maxp;

	maxp = (char *)a + *nbytes -1;
	for ( p=(char *)a ; p <= maxp ; p++ )
	    *p = 0;

	return 0;
}
