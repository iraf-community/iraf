# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	"mtio.h"

# MT_READ_LOCKFILE -- Read the magtape lock file to determine the current
# position of the tape (the lock file is used to record the tape position
# while the device is closed).  If the lock file cannot be accessed or an
# error occurs in reading it, return an undefined tape position.

procedure mt_read_lockfile (mt, file_number, record_number)

int	mt			# MTIO descriptor
int	file_number		# receives current file number
int	record_number		# receives current record number

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

	call mt_lockname (MT_DRIVE(mt), Memc[lockfile], SZ_FNAME)

	# If the lock file cannot be accessed, return an undefined tape
	# position but do not abort.

	iferr (fd = open (Memc[lockfile], READ_ONLY, TEXT_FILE))
	    goto err_

	# Get file number.
	repeat {
	    if (getline (fd, Memc[lbuf]) == EOF)
		goto err_
	} until (strmatch (Memc[lbuf], "^file") > 0)

	ip = stridxs ("=", Memc[lbuf]) + 1
	if (ctoi (Memc[lbuf], ip, file_number) == 0)
	    goto err_

	# Get record number.  We ASSUME that the record entry follows the
	# file entry.

	if (getline (fd, Memc[lbuf]) == EOF)
	    goto err_
	ip = stridxs ("=", Memc[lbuf]) + 1
	if (ctoi (Memc[lbuf], ip, record_number) == 0)
	    goto err_

	call close (fd)
	call sfree (sp)

	return
err_
	# Write a new lock file so that we can update the tape position
	# later (the file must exist after the drive is opened).

	call strcpy ("mt", Memc[lbuf], SZ_LINE)
	call strcat (MT_DRIVE(mt), Memc[lbuf], SZ_LINE)
	call mt_allocate (Memc[lbuf])

	# Return an undefined tape position.
	file_number   = -1
	record_number = -1

	call sfree (sp)
end
