#include "bootlib.h"

/* OS_DELETE -- Delete a file.
 */
os_delete (fname)
char	*fname;
{
	int	status;

	ZFDELE ((PKCHAR *)vfn2osfn (fname, 0), &status);
	return (status);
}
