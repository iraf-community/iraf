/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include "bootlib.h"

/* OS_SETOWNER -- Set the user and group identifications for the file.  This is
 * not a required function and is expected to rarely work when transporting
 * files to a host at a different site.  Nonetheless it is useful when moving
 * files between compatible hosts at a single site, so we make use of it in
 * case it works.
 */
os_setowner (fname, uid, gid)
char	*fname;
int	uid, gid;
{
	return (chown (vfn2osfn(fname,0), uid, gid));
}
