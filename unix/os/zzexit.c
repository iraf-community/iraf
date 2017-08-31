#include <stdlib.h>

#define import_spp
#include <iraf.h>

/*
 * ZZEXIT.C -- Fortran callable exit procedure.  Some systems (e.g. libf2c)
 * require this procedure.  We implement it as a separate library procedure
 * so that it can be replaced by a user exit procedure.
 */
int
exit_ (long int *code)
{
	exit (*code);
	return (XOK);
}
