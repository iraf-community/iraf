/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>

#define	import_kernel
#define	import_knames
#define import_protect
#define import_spp
#include <iraf.h>

#define	PREFIX		".."		/* hidden link for protected files */

/* ZFPROT -- Protect a file from accidental deletion.  In UNIX, this is
 * done by making another link to the file.  If ZFPROT is directed to protect
 * a file, and the file is already protected, the call is ignored.  Similarly,
 * if ZFPROT is directed to remove protection, and the file is not protected,
 * the call is ignored.
 */
ZFPROT (fname, action, status)
PKCHAR	*fname;
XINT	*action, *status;
{
	register char	*p;
	char	link_name[SZ_PATHNAME];
	int	first, link(), chk_prot();
	char	*rindex();

	/* Build up name of link file: "dir/..fname".  This is done by copying
	 * fname to the filename buffer of the link file and truncating the
	 * new filename after the directory prefix (if any).
	 */
	strcpy (link_name, (char *)fname);
	if ((p = rindex (link_name, '/')) != NULL) {
	    *(p+1) = EOS;
	    first = p - link_name + 1;		/* first char after '/' */
	} else {
	    *link_name = EOS;
	    first = 0;
	}

	strcat (link_name, PREFIX);
	strcat (link_name, &((char *)fname)[first]);

	if (access ((char *)fname, 0) == ERR)
	    return (*status = XERR);

	switch (*action) {
	case REMOVE_PROTECTION:
	    if (chk_prot ((char *)fname, link_name) == XNO)
		*status = XOK;
	    else if (unlink (link_name) == ERR)
		*status = XERR;
	    else 
		*status = XOK;
	    return;

	case SET_PROTECTION:
	    if (chk_prot ((char *)fname, link_name) == XYES)
		*status = XOK;
	    else if (link ((char *)fname, link_name) == ERR)
		*status = XERR;
	    else 
		*status = XOK;
	    return;

	default:
	    *status = chk_prot ((char *)fname, link_name);
	    return;
	}
}


/* CHK_PROT -- Determine whether or not a file is protected.
 */
chk_prot (fname, link_name)
char	*fname, *link_name;
{
	int	access();
	struct	stat file1, file2;

	if (access(link_name,0) == ERR)
	    return (XNO);
	else {
	    stat (fname, &file1);
	    stat (link_name, &file2);
	    /* Make sure prefixed file is actually a link to original file */
	    if (file1.st_ino == file2.st_ino && file1.st_dev == file2.st_dev)
		return (XYES);
	    else
		return (XNO);
	}
}
