# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<syserr.h>
include	<fio.h>

# REOPEN -- Reopen a binary file.  Used to gain two or more independent
# sets of buffers to access a binary file.  No protection against two
# file descriptors trying to write to the same part of the file at the
# same time, which may result in loss of data.  The file descriptors and
# buffers of reopened files are independent, but all files accessing the
# same channel share the same channel descriptor (necessary to synchronize
# i/o requests and to maintain a unique file size parameter).

int procedure reopen (fd, mode)

int	fd, mode
pointer	newfp, ffp
int	newfd, fgetfd()
errchk	syserr, malloc, seek
include	<fio.com>

begin
	ffp = fiodes[fd]
	if (fd <= 0 || ffp == NULL)
	    call syserr (SYS_FILENOTOPEN)

	if (FMODE(ffp) == READ_ONLY && mode != READ_ONLY)
	    call filerr (FNAME(ffp), SYS_FREOPNMODE)
	if (FTYPE(ffp) != BINARY_FILE)
	    call filerr (FNAME(ffp), SYS_FREOPNTYPE)

	newfd = fgetfd (FNAME(ffp), mode, BINARY_FILE)
	newfp = fiodes[newfd]

	FDEV(newfp) = FDEV(ffp)
	FBUFSIZE(newfp) = FBUFSIZE(ffp)
	FCHAN(newfp) = FCHAN(ffp)

	# If this is the first reopen, allocate space for a separate channel
	# descriptor and copy the channel descriptor from the original file.

	if (FCD(ffp) == FLCD(ffp)) {
	    call malloc (FCD(ffp), LEN_CHANDES, TY_STRUCT)
	    call amovi (Memi[FLCD(ffp)], Memi[FCD(ffp)], LEN_CHANDES)
	}

	FREFCNT(ffp) = FREFCNT(ffp) + 1			# bump ref count
	FCD(newfp) = FCD(ffp)

	if (mode == APPEND)
	    call seek (newfd, EOFL)

	return (newfd)
end
