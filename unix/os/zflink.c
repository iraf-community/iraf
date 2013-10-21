/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include <stdio.h>
#define	import_kernel
#define	import_knames
#define import_spp
#include <iraf.h>


/* ZFLINK -- Create a file symlink.
 */
int
ZFLINK (
  PKCHAR  *path1,
  PKCHAR  *path2,
  XINT	  *status
)
{
	/*  Create a symlink 'path2' pointing to 'path1'.
	 */
	*status = (symlink ((char *) path1, (char *) path2) < 0) ? XERR : XOK;

	return (*status);
}


/* ZFULNK -- Remove a file symlink.
 */
int
ZFULNK (
  PKCHAR  *path,
  XINT	  *status
)
{
	/*  Remove the link at 'path'.
	 */
	*status = (unlink ((char *) path) < 0) ? XERR : XOK;

	return (*status);
}
