/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include "bootlib.h"

/* OS_CREATEDIR -- Create a new subdirectory.
 */
os_createdir (dirname, mode)
char	*dirname;
int	mode;
{
	if (bdebug)
	    fprintf (stderr, "createdir '%s'\n", dirname);
	return (mkdir (vfn2osfn(dirname,1), mode));
}
