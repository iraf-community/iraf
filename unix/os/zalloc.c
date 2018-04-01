/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <utmpx.h>
#include <pwd.h>

#define import_spp
#define import_alloc
#define	import_kernel
#define	import_knames
#include <iraf.h>

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

#define	ALLOCEXE	"alloc.e"

static int u_allocstat (char *aliases);



/* ZDVALL -- Allocate or deallocate a device.  UNIX does not explicitly support
 * device allocation, so we fake it by setting the device owner and removing
 * group and other permissions.  This requires superuser privilege, hence a
 * separate process HLIB$ALLOC.E is used to set/remove device allocation.
 */
int
ZDVALL (
  PKCHAR  *aliases,		/* list of aliases for device		*/
  XINT	  *allflg,		/* allocate or deallocate device?	*/
  XINT	  *status		/* receives status word			*/
)
{
	PKCHAR	cmd[SZ_LINE+1], nullstr[1];

	extern int ZOSCMD (PKCHAR *oscmd, PKCHAR *stdin_file, PKCHAR  *stdout_file, PKCHAR *stderr_file, XINT *status);


	/* Syntax: $host/hlib/alloc.e -[ad] aliases
	 */
	strcpy ((char *)cmd, irafpath(ALLOCEXE));
	strcat ((char *)cmd, *allflg ? " -a " : " -d ");
	strcat ((char *)cmd, (char *)aliases);

	*nullstr = XEOS;
	(void) ZOSCMD (cmd, nullstr, nullstr, nullstr, status);
	if (*status == DV_ERROR)
	    *status = XERR;

	return (*status);
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
int
ZDVOWN (
  PKCHAR  *device,		/* device name (not a list)	*/
  PKCHAR  *owner,		/* receives owner string	*/
  XINT	  *maxch,		/* max chars out		*/
  XINT	  *status		/* receives allocation status	*/
)
{
	register int	uid;
	char	*dev, devname[SZ_FNAME+1];
	struct	passwd *pw;
	struct	stat fi;


	/* Get device pathname. */
	dev = (char *)device;
	if (dev[0] == '/') {
	    strcpy (devname, dev);

        } else if (dev[0] == '~' && dev[1] == '/') {
            /* User home directory relative pathname. */
            struct  passwd *pwd;
            pwd = getpwuid (getuid());
            if (pwd != NULL) {
                strcpy (devname, pwd->pw_dir);
                strcat (devname, &dev[1]);
                endpwent();
	    }
	} else {
	    sprintf (devname, "/dev/%s", dev);
	    if (access (devname, 0) == ERR)
		sprintf (devname, "/dev/rmt/%s", dev);
	}

	if (stat (devname, &fi) == ERR) {
	    *status = XERR;
	    return (XERR);
	}

	uid = fi.st_uid;
	*owner = XEOS;

	if (uid == 0)
	    *status = DV_DEVFREE;
	else if (uid == getuid())
	    *status = DV_DEVALLOC;
	/* else if (!loggedin (uid)) */
	else if (u_allocstat ((char *)device) == DV_DEVFREE)
	    *status = DV_DEVFREE;
	else {
	    if ((pw = getpwuid (uid)) == NULL)
		sprintf ((char *)owner, "%d", uid);
	    else
		strncpy ((char *)owner, pw->pw_name, *maxch);
	    *status = DV_DEVINUSE;
	}

	return (*status);
}


/* LOGGEDIN -- Return 1 if uid is logged in, else 0.
 */
int
loggedin (int uid)
{
	struct	utmpx ubuf;
	struct	passwd *pw;
	FILE	*ufp;

	if ((ufp = fopen ("/var/run/utmp", "r")) == NULL) {
	    printf ("zdvown: cannot open /var/run/utmp\n");
	    return (1);
	}

	if ((pw = getpwuid (uid)) == NULL) {
	    fclose (ufp);
	    return (0);
	}

	do {
	    if (fread (&ubuf, sizeof (struct utmpx), 1, ufp) == (size_t) 0) {
		fclose (ufp);
		return (0);
	    }
	} while (strncmp (ubuf.ut_user, pw->pw_name, 8) != 0);

	fclose (ufp);

	return (1);
}


/* U_ALLOCSTAT -- Call alloc.e to get the device status.  Currently, this has
 * to be done by a priviledged process as the process table is used in some
 * cases.
 */
static int
u_allocstat (
  char	*aliases		/* list of aliases for device		*/
)
{
	PKCHAR	cmd[SZ_LINE+1], nullstr[1];
	XINT	x_status;

	extern int ZOSCMD(PKCHAR *oscmd, PKCHAR *stdin_file, PKCHAR  *stdout_file, PKCHAR *stderr_file, XINT *status);


	/* Syntax: $host/hlib/alloc.e -s aliases
	 */
	strcpy ((char *)cmd, irafpath(ALLOCEXE));
	strcat ((char *)cmd, " -s ");
	strcat ((char *)cmd, aliases);

	*nullstr = XEOS;
	(void) ZOSCMD (cmd, nullstr, nullstr, nullstr, &x_status);
	if (x_status == DV_ERROR)
	    x_status = XERR;

	return (x_status);
}
