/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <utmpx.h>
#include <pwd.h>
#include <sys/stat.h>
#include <ctype.h>
#include <string.h>

#define	import_spp
#define	import_alloc
#define	import_knames
#include <iraf.h>

/*
 * ALLOC -- Unix task to allocate and deallocate devices given their generic
 * name.  These names are associated with special files in the ALLOCFILE file.
 * Allocation is accomplished by setting the device file owner and permissions
 * for the /dev entries associated with a particular logical device.
 * Although we are called by the IRAF kernel, we are implemented as a task
 * rather than as a function since super user permission is required to modify
 * directory entries in /dev.
 *
 * usage:
 *	$hbin/alloc.e -a aliases		allocate device
 *	$hbin/alloc.e -d aliases		deallocate device
 *	$hbin/alloc.e -s aliases		get allocation status
 *
 * Here, "aliases" are the names of all entries in /dev for the physical device,
 *   e.g., "mt8", "rmt8", "nrmt8", etc.  For security reasons, only device
 *   files in /dev can be allocated, and the user must already have RW perm
 *   on the device file.  Allocating the file simply changes the file
 *   ownership to the uid of the caller, and removes access permissions for
 *   group and world.
 *
 * NOTE: THIS TASK MUST BE INSTALLED IN HLIB WITH OWNER=ROOT AND
 *   "set uid on execution" PERMISSION (see mkpkg).
 */

#define	NSFILES		30		/* max number spec files assoc w/dev */
#define	RWOWN		0600		/* -rw------			*/
#define	RWALL		0666		/* -rw-rw-rw			*/

struct file {				/* special files for "device"	*/
	char f_name[SZ_FNAME];		/* file name			*/
	struct stat f_sbuf;		/* stat buffer			*/
} sfiles[NSFILES];

int	debug=0;
int	nsfiles;			/* number of special files	*/
int	mode;				/* 07 mode, ie, 04, 02, or 06	*/


/* System prototypes.
*/
int findsfs (char *argv[]);
int dealloc (char *argv[]);
int alloc (char	*argv[], int statonly);

extern  int	uid_executing (int uid);



int main (int argc, char *argv[])
{
	int	iexit = DV_ERROR;

	if (geteuid()) {
	    fprintf (stderr,
		"Error: uid of $hbin/alloc.e must be set to 0 (root)\n");
	    fprintf (stderr, "(rerun install script $hlib/install, or)\n");
	    fprintf (stderr, "(login as root: cd $hbin; chown 0 alloc.e)\n");
	    exit (DV_ERROR);
	} else if (argc < 3) {
	    fprintf (stderr, "alloc.e called with invalid argument list\n");
	    exit (DV_ERROR);
	}

	if (strcmp (argv[1], "-a") == 0)
	    iexit = alloc (&argv[2], 0);
	else if (strcmp (argv[1], "-s") == 0)
	    iexit = alloc (&argv[2], 1);
	else
	    iexit = dealloc (&argv[2]);

	exit (iexit);
}


/* ALLOC -- Allocate device with given generic name if its owner is not
 * logged in.
 */
int
alloc (
  char	*argv[],
  int    statonly		/* if set, just return device status */
)
{
	register int ruid, mode, i;
	register struct file *fp;
	int	rgid;

	if (findsfs (argv) == 0)
	    return (DV_ERROR);

	if (debug)
	    printf ("allocate %d files\n", nsfiles);

	for (i=0;  i < nsfiles;  i++) {
	    fp = &sfiles[i];
	    ruid = fp->f_sbuf.st_uid;
	    mode = fp->f_sbuf.st_mode;

	    /* We don't really care if the uid when the device is not
	     * allocated is root, bin, or whatever, so long at it is some
	     * system uid.
	     */
	    if (ruid < 10)
		ruid = 0;

	    if (ruid == 0 && (mode & 06) != 06) {
		if (!statonly)
		    printf ("rw access to %s is denied\n", fp->f_name);
		return (DV_DEVINUSE);
	    } else if (ruid && uid_executing(ruid)) {
		if (ruid != getuid()) {
		    if (!statonly)
			printf ("%s already allocated to %s\n",
			    fp->f_name, (getpwuid(ruid))->pw_name);
		    return (DV_DEVINUSE);
		} else
		    return (statonly ? DV_DEVALLOC : XOK);
	    }
	}

	if (statonly)
	    return (DV_DEVFREE);

	ruid = getuid();
	rgid = getgid();

	for (i=0;  i < nsfiles;  i++) {
	    fp = &sfiles[i];
	    if (debug)
		printf ("alloc file `%s'\n", fp->f_name);
	    if (chmod (fp->f_name, RWOWN) == -1)
		printf ("cannot chmod `%s'\n", fp->f_name);
	    if (chown (fp->f_name, ruid, rgid) == -1)
		printf ("cannot chown `%s'\n", fp->f_name);
	}

	return (XOK);
}


/* DEALLOC -- Deallocate device with given generic name if real uid owns all
 * sfiles.
 */
int
dealloc (char *argv[])
{
	register int	uid, ruid, i;
	register struct	file *fp;

	if (findsfs (argv) == 0)
	    return (DV_ERROR);

	if (debug)
	    printf ("deallocate %d files\n", nsfiles);

	ruid = getuid();
	if (ruid)
	    for (i=0;  i < nsfiles;  i++) {
		fp = &sfiles[i];
		uid = fp->f_sbuf.st_uid;
		if (uid && uid != ruid)
		    return (DV_ERROR);
	    }

	for (i=0;  i < nsfiles;  i++) {
	    fp = &sfiles[i];
	    if (fp->f_sbuf.st_uid == 0)
		continue;
	    if (debug)
		printf ("dealloc file `%s'\n", fp->f_name);
	    if (chmod (fp->f_name, RWALL) == -1)
		printf ("cannot chmod `%s'\n", fp->f_name);
	    if (chown (fp->f_name, 0, 0) == -1)
		printf ("cannot chown `%s'\n", fp->f_name);
	}

	return (XOK);
}


/* FINDSFS -- Fill in sfiles table with special file names associated with
 * device with given generic name.
 */
int
findsfs (char *argv[])
{
	register struct file *fp;
	register char	*argp, *ip;
	char	*fname;

	for (nsfiles=0;  (argp = argv[nsfiles]);  nsfiles++) {
	    fp = &sfiles[nsfiles];
	    for (ip=fname=argp;  *ip;  ip++)
		if (!isalnum (*ip))
		    fname = ip + 1;
	    if (*fname == '\0') {
		printf ("alloc: cannot fstat %s\n", fname);
		continue;
	    }

	    sprintf (fp->f_name, "/dev/%s", fname);
	    if (stat (fp->f_name, &fp->f_sbuf) == -1) {
		sprintf (fp->f_name, "/dev/rmt/%s", fname);
		if (stat (fp->f_name, &fp->f_sbuf) == -1) {
		    printf ("alloc: cannot fstat %s\n", fp->f_name);
		    continue;
		}
	    }
	}

	return (nsfiles);
}


#ifdef DEFUNCT
/* This is no longer used since we now read the process table instead. */
/* LOGGEDIN -- Return 1 if uid is logged in, else 0.
 */
int
loggedin (int uid)
{
	register int i;
	static	int uidcache[10];
	static	int nuid = 0;
	struct	utmpx ubuf;
	struct	passwd *pw, *getpwuid();
	FILE	*ufp;

	for (i=0;  i < nuid;  i++)
	    if (uid == uidcache[i])
		return (1);

	if ((ufp = fopen ("/var/run/utmp", "r")) == NULL) {
	    printf ("cannot open utmp file\n");
	    exit (DV_ERROR);
	}

	if ((pw = getpwuid (uid)) == NULL) {
	    printf ("uid %d not in passwd file\n", uid);
	    exit (DV_ERROR);
	}

	do {
	    if (fread (&ubuf, sizeof (struct utmpx), 1, ufp) == NULL)
		return (0);
	} while (strncmp (ubuf.ut_user, pw->pw_name, 8) != 0);

	if (nuid < 10)
	    uidcache[nuid++] = uid;

	return (1);
}
#endif
