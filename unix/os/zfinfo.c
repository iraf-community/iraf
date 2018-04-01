/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <pwd.h>

#define	import_kernel
#define	import_knames
#define import_spp
#define import_finfo
#include <iraf.h>

/* ZFINFO -- Get information describing the named file.  Access times
 * are returned in units of seconds since 00:00:00 01-Jan-80, local time.
 */
int
ZFINFO (
  PKCHAR  *fname,
  XLONG	  *finfo_struct,
  XINT	  *status
)
{
	struct	stat osfile;
	struct	_finfo *fs;
	time_t	gmt_to_lst(time_t gmt);

	/* Get UNIX file info.
	 */
	fs = (struct _finfo *)finfo_struct;
	if (stat ((char *)fname, &osfile) == ERR) {
	    *status = XERR;
	    return (XERR);
	}

	/* Determine file type.
	 */
	if (osfile.st_mode & S_IFDIR)
	    fs->fi_type = FI_DIRECTORY;
	else if (osfile.st_mode & S_IEXEC)
	    fs->fi_type = FI_EXECUTABLE;
	else if (osfile.st_mode & S_IFREG)
	    fs->fi_type = FI_REGULAR;
	else
	    fs->fi_type = FI_SPECIAL;

	/* Set file size (in bytes), access times, and file permission bits.
	 * Times must be converted from GMT epoch 1970 to local standard time,
	 * epoch 1980.
	 */
	fs->fi_size  = osfile.st_size;
	fs->fi_atime = gmt_to_lst (osfile.st_atime);
	fs->fi_mtime = gmt_to_lst (osfile.st_mtime);
	fs->fi_ctime = gmt_to_lst (osfile.st_ctime);

 	/* Encode file access permission bits.
	 */
	{
	    static	int osbits[] = { 0400, 0200, 040, 020, 04, 02 };
	    int		bit;

	    for (bit=0, fs->fi_perm=0;  bit < 6;  bit++)
		fs->fi_perm |= (osfile.st_mode & osbits[bit]) ? 1<<bit: 0;
	}

 	/* Get owner name.  Once the owner name string has been retrieved
	 * for a particular (system wide unique) UID, cache it, to speed
	 * up multiple requests for the same UID.
	 */
	{
	    static	int uid = 0;
	    static	char owner[SZ_OWNERSTR+1];
	    struct	passwd *pw;

	    if (osfile.st_uid == uid)
		strncpy ((char *)fs->fi_owner, owner, SZ_OWNERSTR);
	    else {
		setpwent();
		pw = getpwuid (osfile.st_uid);
		endpwent();

		if (pw == NULL)
		    sprintf ((char *)fs->fi_owner, "%d", osfile.st_uid);
		else {
		    strncpy (owner, pw->pw_name, SZ_OWNERSTR);
		    strncpy ((char *)fs->fi_owner, owner, SZ_OWNERSTR);
		    uid = osfile.st_uid;
		}
	    }
	    ((char *)fs->fi_owner)[SZ_OWNERSTR] = EOS;
	}

	*status = XOK;

	return (*status);
}
