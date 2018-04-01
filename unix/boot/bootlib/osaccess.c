/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <string.h>
#include "bootlib.h"

/* OS_ACCESS -- Determine if file is accessible with the given access mode
 * and type.  Returns YES (1) or NO (0).
 */
int
os_access (
  char	*fname,
  int	mode,
  int	type
)
{
	PKCHAR	osfn[SZ_PATHNAME+1];
	XINT	status, xmode=mode, xtype=type;

	extern  int ZFACSS(PKCHAR *fname, XINT *mode, XINT *type, XINT *status);


	strcpy ((char *)osfn, vfn2osfn(fname,0));
	ZFACSS (osfn, &xmode, &xtype, &status);

	return (status);
}
