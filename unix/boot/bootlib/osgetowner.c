/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <sys/types.h>
#include <sys/stat.h>
#include "bootlib.h"


/* OS_GETOWNER -- Get the user and group identifications for a file.  This is
 * not a required function and is expected to rarely work when transporting
 * files to a host at a different site.  Nonetheless it is useful when moving
 * files between compatible hosts at a single site, so we make use of it in
 * case it works.  It is sufficient to merely set uid and gid to 0 and return.
 */
void
os_getowner (
  char	*fname,
  int	*uid, 
  int	*gid 
)
{
	struct	stat fi;

	if (stat (vfn2osfn(fname,0), &fi) != -1) {
	    *uid = fi.st_uid;
	    *gid = fi.st_gid;
	}
}
