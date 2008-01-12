/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <utmp.h>
#include <pwd.h>

#define import_spp
#define import_alloc
#define import_kernel
#define import_knames
#include <iraf.h>

#include "zos.h"

/*
 * ZALLOC.C -- Device allocation interface.  Requires the dev$devices table,
 * which is read by the high level code before we are called.
 *
 *	zdvall (device, allflg, status)		allocate/deallocate device
 *	zdvown (device, owner, maxch, status)	query device allocation
 *
 * Status returns (import_alloc):
 *
 *	OK			operation successful
 *	ERR			cannot allocate device
 *	DV_DEVALLOC		device is already allocated
 *	DV_DEVINUSE		device is in use by someone else
 *	DV_DEVFREE		device is free and can be allocated
 *	
 * The DEVICE name is a list of aliases for the physical device.  On UNIX there
 * can be multiple elements in the list (e.g., for a tape drive), but on other
 * systems there may never be more than one device name.  The elements of the
 * list are delimited by whitespace.  On host systems that do not support
 * multiple device aliases, the kernel may assume that DEVICE is a scalar.
 */

#define ALLOCEXE	"alloc.e"

static int u_allocstat ( const char * );

/* ZDVALL -- Allocate or deallocate a device.  UNIX does not explicitly support
 * device allocation, so we fake it by setting the device owner and removing
 * group and other permissions.  This requires superuser privilege, hence a
 * separate process HLIB$ALLOC.E is used to set/remove device allocation.
 */
/* aliases : list of aliases for device		*/
/* allflg  : allocate or deallocate device?	*/
/* status  : receives status word		*/
int ZDVALL ( PKCHAR *aliases, XINT *allflg, XINT *status )
{
	PKCHAR	cmd[SZ_LINE+1], nullstr[1];

	/* Syntax: $host/bin/alloc.e -[ad] aliases
	 */
	safe_strcpy ((char *)cmd, SZ_LINE+1, irafpath(ALLOCEXE));
	safe_strcat ((char *)cmd, SZ_LINE+1, *allflg ? " -a " : " -d ");
	safe_strcat ((char *)cmd, SZ_LINE+1, (const char *)aliases);

	*nullstr = XEOS;
	ZOSCMD (cmd, nullstr, nullstr, nullstr, status);
	if (*status == DV_ERROR)
	    *status = XERR;

	return *status;
}


/* ZDVOWN -- Query device allocation.  Tells whether or not the named device
 * is allocated, and if so returns the "name" of the owner in the owner
 * string.  Just what the "name" string is is not precisely defined, it is
 * merely printed for the user to tell them the status of the device.
 * Note that the device is not considered to be allocated if the owner
 * is not currently logged in.
 *
 * Device files may be specified by a full pathname, as a user directory
 * relative pathname, or by the device name in /dev or /dev/rmt.
 */
/* device : device name (not a list)	*/
/* owner  : receives owner string	*/
/* maxch  : max chars out		*/
/* status : receives allocation status	*/
int ZDVOWN ( PKCHAR *device, PKCHAR *owner, XINT *maxch, XINT *status )
{
	char devname[SZ_FNAME+1];
	const char *dev;
	struct stat fi;
	struct passwd *pw;
	size_t bufsize = *maxch + 1;
	int uid;

	/* Get device pathname. */
	dev = (const char *)device;
	if (dev[0] == '/') {
	    safe_strcpy (devname, SZ_FNAME+1, dev);

        } else if (dev[0] == '~' && dev[1] == '/') {
            /* User home directory relative pathname. */
            struct  passwd *pwd;
            pwd = getpwuid (getuid());
            if (pwd != NULL) {
                safe_strcpy (devname, SZ_FNAME+1, pwd->pw_dir);
                safe_strcat (devname, SZ_FNAME+1, &dev[1]);
                endpwent();
	    }
	} else {
	    snprintf (devname, SZ_FNAME+1, "/dev/%s", dev);
	    if (access (devname, 0) == ERR)
		snprintf (devname, SZ_FNAME+1, "/dev/rmt/%s", dev);
	}

	if (stat (devname, &fi) == ERR) {
	    *status = XERR;
	    return *status;
	}

	uid = fi.st_uid;
	*owner = XEOS;

	if (uid == 0)
	    *status = DV_DEVFREE;
	else if (uid == getuid())
	    *status = DV_DEVALLOC;
	/* else if (!loggedin (uid)) */
	else if (u_allocstat ((const char *)device) == DV_DEVFREE)
	    *status = DV_DEVFREE;
	else {
	    if ((pw = getpwuid (uid)) == NULL)
		snprintf ((char *)owner, bufsize, "%d", uid);
	    else
		safe_strcpy ((char *)owner, bufsize, pw->pw_name);
	    *status = DV_DEVINUSE;
	}

	return *status;
}


/* LOGGEDIN -- Return 1 if uid is logged in, else 0.
 */
int loggedin ( int uid )
{
	struct	utmp ubuf;
	struct	passwd *pw;
	FILE	*ufp;

	if ((ufp = fopen ("/etc/utmp", "r")) == NULL) {
	    printf ("zdvown: cannot open /etc/utmp\n");
	    return (1);
	}

	if ((pw = getpwuid (uid)) == NULL) {
	    fclose (ufp);
	    return (0);
	}

	do {
	    if (fread (&ubuf, sizeof (struct utmp), 1, ufp) == 0) {
		fclose (ufp);
		return (0);
	    }
	} while (strncmp (ubuf.ut_name, pw->pw_name, 8) != 0);

	fclose (ufp);
	return (1);
}


/* U_ALLOCSTAT -- Call alloc.e to get the device status.  Currently, this has
 * to be done by a priviledged process as the process table is used in some
 * cases.
 */
/* aliases : list of aliases for device		*/
static int u_allocstat ( const char *aliases )
{
	PKCHAR	cmd[SZ_LINE+1], nullstr[1];
	XINT	x_status;

	/* Syntax: $host/bin/alloc.e -s aliases
	 */
	safe_strcpy ((char *)cmd, SZ_LINE+1, irafpath(ALLOCEXE));
	safe_strcat ((char *)cmd, SZ_LINE+1, " -s ");
	safe_strcat ((char *)cmd, SZ_LINE+1, aliases);

	*nullstr = XEOS;
	ZOSCMD (cmd, nullstr, nullstr, nullstr, &x_status);
	if (x_status == DV_ERROR)
	    x_status = XERR;

	return (x_status);
}
