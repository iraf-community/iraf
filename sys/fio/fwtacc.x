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

int	fd				#I file we are trying to open
char	fname[ARB]			#I name of file

bool	locked
pointer	sp, osfn
int	perm, delay, chan, status, ofd
long	fi[LEN_FINFO]

int	access(), finfo()
bool	streq(), envgetb()
errchk	filerr, fmapfn
include	<fio.com>
define	noacc_ 91

begin
	call smark (sp)
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

	# Test if we the open failed because we cannot physically open any
	# more files.

	call fmapfn (SOMEFILE, Memc[osfn], SZ_PATHNAME)
	call zopntx (Memc[osfn], READ_ONLY, chan)
	if (chan == ERR)
	    call filerr (fname, SYS_FTOOMANYFILES)
	else
	    call zclstx (chan, status)

	# If the file exists, we cannot access it, and there is no temporary
	# read or write lock in place on the file, then the file cannot be
	# accessed.

	perm = FI_PERM(fi)
	locked = false
	if (and (fflags[fd], FF_READ) != 0)
	    locked = locked || (and (perm, FF_RDLOCK) != 0)
	if (and (fflags[fd], FF_WRITE) != 0)
	    locked = locked || (and (perm, FF_WRLOCK) != 0)

	# If filewait is enabled, wait for the file to become accessible.
	if (envgetb ("filewait") && locked) {
	    call putline (STDERR, "Waiting for access to file '")
	    call putline (STDERR, fname)
	    call putline (STDERR, "'\n")

	    for (delay=MIN_DELAY;  delay > 0;  delay=delay+INC_DELAY) {
		call tsleep (min (delay, MAX_DELAY))

		if (access (fname,0,0) == NO)
		    call filerr (fname, SYS_FOPNNEXFIL)
		else if (access (fname,FMODE(fp),0) == YES) {
		    call sfree (sp)
		    return
		}
		
		# Verify that the file is still locked.
		if (finfo (fname, fi) == ERR)
		    call filerr (fname, SYS_FOPNNEXFIL)

		locked = false
		if (and (fflags[fd], FF_READ) != 0)
		    locked = locked || (and (perm, FF_RDLOCK) != 0)
		if (and (fflags[fd], FF_WRITE) != 0)
		    locked = locked || (and (perm, FF_WRLOCK) != 0)
		
		if (!locked)
		    break
	    }
	} 
noacc_
	call sfree (sp)
    	call filerr (fname, SYS_FWTNOACC)
end
