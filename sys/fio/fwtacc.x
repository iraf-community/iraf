# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<syserr.h>
include <finfo.h>
include	<fio.h>

define	MIN_DELAY	1
define	MAX_DELAY	60
define	INC_DELAY	1
define	SOMEFILE	"hlib$iraf.h"


# FWTACC -- Called if a file open fails.  Determine if failure to open file
# was due to the file already being open by another task.  If so, and file
# waiting is enabled in the environment, wait for file to become accessible,
# otherwise take error action.

procedure fwtacc (fd, fname)

int	fd, ofd
char	fname[ARB]

bool	read_access
pointer	sp, osfn, myname
int	perm, delay, chan, status
long	fi[LEN_FINFO]

int	access(), finfo(), and()
bool	streq(), envgetb()
errchk	filerr, fmapfn
include	<fio.com>
define	noacc_ 91

begin
	call smark (sp)
	call salloc (myname, FI_SZOWNER, TY_CHAR)
	call salloc (osfn, SZ_PATHNAME, TY_CHAR)

	fp = fiodes[fd]
	if (FMODE(fp) == NEW_FILE)
	    call filerr (fname, SYS_FOPEN)

	# If file is blocked for some reason because it is already open
	# by this process, waiting would result in a deadlock.

	for (ofd=FIRST_FD;  ofd <= LAST_FD;  ofd=ofd+1)
	    if (ofd != fd && fiodes[ofd] != NULL)
		if (streq (fname, FNAME(fiodes[ofd])))
		    call filerr (fname, SYS_FWTOPNFIL)

	# If file waiting is enabled, the file exists and has write permission
	# for us but is not writable at the moment, wait for the file to
	# become available.  FINFO is used to determine the "permanent"
	# permissions for the file and the file owner.  ACCESS determines the
	# runtime accessibility of the file.  The permanent file protection may
	# permit writing but the file may not be accessible for writing at
	# runtime if opened for exclusive access by another process.

	if (finfo (fname, fi) == ERR)
	    call filerr (fname, SYS_FOPNNEXFIL)

	# Directory files are not accessible as files.
	if (FI_TYPE(fi) == FI_DIRECTORY)
	    goto noacc_

	# Test if we the open failed because we cannot physically open any more
	# files.

	call fmapfn (SOMEFILE, Memc[osfn], SZ_PATHNAME)
	call zopntx (Memc[osfn], READ_ONLY, chan)
	if (chan == ERR)
	    call filerr (fname, SYS_FTOOMANYFILES)
	else
	    call zclstx (chan, status)

	call fowner (fname, Memc[myname], FI_SZOWNER)
	read_access = (FMODE(fiodes[fd]) == READ_ONLY)
	perm = FI_PERM(fi)

	if (streq (FI_OWNER(fi), Memc[myname])) {
	    # We are the owner of the file.

	    if (read_access && and (perm, FI_ROWNER) == 0)
	        goto noacc_
	    else if (and (perm, FI_WOWNER) == 0)
		goto noacc_

	} else {
	    # The file belongs to someone else.  We cannot discriminate between
	    # group and world permission, so ignore group permission and abort
	    # if the file does not have the indicated world permission.

	    if (read_access && and (perm, FI_RWORLD) == 0)
	        goto noacc_
	    else if (and (perm, FI_WWORLD) == 0)
		goto noacc_
	}

	# If filewait is enabled, wait for the file to become accessible.

	if (envgetb ("filewait")) {
	    call putline (STDERR, "Waiting for access to file '")
	    call putline (STDERR, fname)
	    call putline (STDERR, "'\n")

	    for (delay=MIN_DELAY;  delay > 0;  delay=delay+INC_DELAY) {
		call tsleep (min (delay, MAX_DELAY))
		if (access (fname,0,0) == NO)
		    call filerr (fname, SYS_FOPNNEXFIL)
		else if (access (fname,FMODE(fp),0) == YES)
		    return
	    }
	} 
noacc_
	call sfree (sp)
    	call filerr (fname, SYS_FWTNOACC)
end
