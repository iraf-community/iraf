/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <dirent.h>
#include <sys/types.h>
#include <sys/stat.h>


/* UID_EXECUTING -- Search the process table to determine if the given UID
 * belongs to any currently running processes.  This is straightfoward for
 * Solaris since each process has a file entry in /proc.
 */
int
uid_executing (int uid)
{
	register struct dirent *direntp;
	register DIR *dirp;
	char fname[256];
	struct stat st;

	dirp = opendir ("/proc");
	while ((direntp = readdir(dirp)) != NULL) {
	    sprintf (fname, "/proc/%s", direntp->d_name);
	    if (stat (fname, &st))
		return (0);
	    else if (st.st_uid == uid)
		return (1);
	}
	(void) closedir (dirp);

	return (0);
}
