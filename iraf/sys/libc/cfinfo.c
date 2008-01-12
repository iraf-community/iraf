/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_xnames
#define import_finfo
#include <iraf.h>

/* C_FINFO -- FIO get directory info for named file.
 */
/* fname : name of file to be opened */
/* fi    : finfo structure (output)  */
int c_finfo ( const char *fname, struct _finfo *fi )
{
	int status;

	iferr (status = FINFO (c_sppstr(fname), (XLONG *)fi)) {
	    status = ERR;
	} else if (status != XERR) {
	    c_strpak ((XCHAR *)fi->fi_owner, fi->fi_owner, SZ_OWNERSTR);
	    status = OK;
	} else
	    status = ERR;

	return (status);
}
