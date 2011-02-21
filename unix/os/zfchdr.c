/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#define	import_kernel
#define	import_knames
#define import_spp
#include <iraf.h>

extern	char oscwd[];


/* ZFCHDR -- Change the current working directory.  Save directory name,
 * excluding the trailing "/", in oscwd so that a subsequent call to ZFGCWD
 * will be able to return directory name without a big hassle.
 */
int
ZFCHDR (
  PKCHAR  *newdir,
  XINT	  *status
)
{
	register char *ip, *op;
	char	dirname[SZ_PATHNAME];


	/* Change pathnames like "a/b/c/" to "a/b/c".
	 */
	for (ip=(char *)newdir, op=dirname;  (*op = *ip++) != EOS;  op++)
	    ;
	if ((*(op-1) == '/') && (op - dirname > 1))
	    *(op-1) = EOS;

	/* Ask UNIX to change the cwd to newdir.
	 */
	if (chdir (dirname) == ERR) {
	    *status = XERR;

	} else if (dirname[0] == '/') {
	    /* Save pathname of directory.
	     */
	    strcpy (oscwd, dirname);
	    *status = XOK;

	} else {
	    /* Concatenate subdir name to current directory pathname.
	     */
	    for (op=oscwd;  *op;  op++)
		;
	    if (*(op-1) != '/')
		*op++ = '/';
	    for (ip=dirname;  (*op++ = *ip++);  )
		;
	}

	return (*status);
}
