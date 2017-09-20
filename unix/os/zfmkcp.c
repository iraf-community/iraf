/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#define	import_kernel
#define	import_knames
#define import_protect
#define import_spp
#include <iraf.h>

/* ZFMKCP -- Make a null length copy of a file.  The new file inherits all
 * attributes of the original file except the file owner (the copy belongs to
 * the owner of the process which called us), the file size (this will be 0
 * after the zfmkcp), and the owner write permission bit (the new file has
 * to be writable by the owner to be useful).
 *
 * Since file protection is implemented by special techniques on UNIX,
 * we must take special measures to pass the file protection attribute to
 * the new copy.
 */
int
ZFMKCP (
  PKCHAR  *osfn,
  PKCHAR  *new_osfn,
  XINT	  *status
)
{
	struct	stat statbuf;
	int	fd, mode;
	XINT	prot;

	extern  int ZFPROT(PKCHAR *fname, XINT *action, XINT *status);


	/* Get directory information for the old file.  Most of the file
	 * attributes reside in the st_mode field.
	 */
	if (stat ((char *)osfn, &statbuf) == ERR) {
	    *status = XERR;
	    return (XERR);
	}

	mode = statbuf.st_mode;

	/* Create new file using mode bits from the existing file.
	 */
	if ((fd = creat ((char *)new_osfn, mode | 0600)) == ERR) {
	    *status = XERR;
	    return (XERR);
	} else
	    close (fd);

	/* Add file protection if the original file is protected.  If new file
	 * cannot be protected delete new file and return ERR.
	 */
	prot = QUERY_PROTECTION;
	ZFPROT (osfn, &prot, status);
	if (*status == XYES) {
	    prot = SET_PROTECTION;
	    ZFPROT (new_osfn, &prot, status);
	}

	if (*status == XERR)
	    unlink ((char *)new_osfn);

	return (XOK);
}
