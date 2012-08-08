# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	"mtio.h"

# MT_READ_LOCKFILE -- Read the magtape lock file to determine the current
# position of the tape (the lock file is used to record the tape position
# while the device is closed).  If the lock file cannot be accessed or an
# error occurs in reading it, return an undefined tape position.

procedure mt_read_lockfile (mtname, mt)

char	mtname[ARB]		#I device name
int	mt			#I MTIO descriptor

int	fd, ip
pointer	sp, lockfile, lbuf
int	strmatch(), stridxs(), ctoi(), open(), getline()
errchk	open, getline
include	"mtio.com"
define	err_ 91

begin
	call smark (sp)
	call salloc (lockfile, SZ_FNAME, TY_CHAR)
	call salloc (lbuf, SZ_LINE, TY_CHAR)

	# If the lock file cannot be accessed, return an undefined tape
	# position but do not abort.

	#call mt_lockname (MT_LKNAME(mt), Memc[lockfile], SZ_FNAME)
	call strcpy (MT_LKNAME(mt), Memc[lockfile], SZ_FNAME)
	iferr (fd = open (Memc[lockfile], READ_ONLY, TEXT_FILE)) {
	    fd = ERR
	    goto err_
	}

	# Get file number.
	repeat {
	    if (getline (fd, Memc[lbuf]) == EOF)
		goto err_
	} until (strmatch (Memc[lbuf], "^file") > 0)
	ip = stridxs ("=", Memc[lbuf]) + 1
	if (ctoi (Memc[lbuf], ip, MT_FILNO(mt)) == 0)
	    goto err_

	# Get record number.
	if (getline (fd, Memc[lbuf]) == EOF)
	    goto err_
	ip = stridxs ("=", Memc[lbuf]) + 1
	if (ctoi (Memc[lbuf], ip, MT_RECNO(mt)) == 0)
	    goto err_

	# Get total files on tape.
	if (getline (fd, Memc[lbuf]) == EOF)
	    goto err_
	ip = stridxs ("=", Memc[lbuf]) + 1
	if (ctoi (Memc[lbuf], ip, MT_NFILES(mt)) == 0)
	    goto err_

	# Get amount of tape used.
	if (getline (fd, Memc[lbuf]) == EOF)
	    goto err_
	ip = stridxs ("=", Memc[lbuf]) + 1
	if (ctoi (Memc[lbuf], ip, MT_TAPEUSED(mt)) == 0)
	    goto err_

	# Get pflags.
	if (getline (fd, Memc[lbuf]) == EOF)
	    goto err_
	ip = stridxs ("=", Memc[lbuf]) + 1
	if (ctoi (Memc[lbuf], ip, MT_PFLAGS(mt)) == 0)
	    goto err_

	call close (fd)
	call sfree (sp)

	return
err_
	if (fd != ERR)
	    call close (fd)

	# Write a new lock file so that we can update the tape position
	# later (the file must exist after the drive is opened).

	call mtallocate (mtname)

	# Return an undefined tape position.
	MT_FILNO(mt) = -1
	MT_RECNO(mt) = -1

	call sfree (sp)
end
