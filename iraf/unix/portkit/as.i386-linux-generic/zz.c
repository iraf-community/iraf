/* Compile with gcc -S to get demo assembler code.
 */
#include <setjmp.h>

#define import_spp
#include <iraf.h>

int zsvjmp_( XPOINTER *buf, XINT *status )
{
	*status = 0;
	((XINT **)buf)[0] = status;
	return sigsetjmp ((void *)((XINT **)buf+1),0);
}
