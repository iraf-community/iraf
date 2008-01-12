/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <string.h>
#include "bootlib.h"

/* OS_ACCESS -- Determine if file is accessible with the given access mode
 * and type.  Returns YES (1) or NO (0).
 */
int os_access ( const char *fname, int mode, int type )
{
	PKCHAR osfn[SZ_PATHNAME+1];
	XINT x_mode,x_type,status;

	x_mode = mode;
	x_type = type;
	safe_strcpy ((char *)osfn, SZ_PATHNAME+1, vfn2osfn(fname,0));
	ZFACSS (osfn, &x_mode, &x_type, &status);
	return (status);
}
