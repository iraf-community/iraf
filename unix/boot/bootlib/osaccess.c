/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include "bootlib.h"

/* OS_ACCESS -- Determine if file is accessible with the given access mode
 * and type.  Returns YES (1) or NO (0).
 */
os_access (fname, mode, type)
char	*fname;
int	mode;
int	type;
{
	PKCHAR	osfn[SZ_PATHNAME+1];
	int	status;

	strcpy ((char *)osfn, vfn2osfn(fname,0));
	ZFACSS (osfn, &mode, &type, &status);
	return (status);
}
