/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <sys/stat.h>
#include "bootlib.h"

/* OS_CREATEDIR -- Create a new subdirectory.
 */
int os_createdir ( const char *dirname, int mode )
{
	if (bdebug)
	    fprintf (stderr, "createdir '%s'\n", dirname);
	return (mkdir (vfn2osfn(dirname,1), mode));
}
