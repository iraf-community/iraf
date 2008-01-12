# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<syserr.h>
include	<config.h>
include	<fio.h>

.help fgdev_param
.nf ________________________________________________________________________
FGDEV_PARAM -- Get device parameters (block size, optimum buffer size)
and set up file descriptor accordingly.  Called by FILOPN and FINIT.

	FSTT_BLKSIZE		Device block size:
				   >= 1 if block structured device
				   == 0 if streaming device (terminal,
					pipe, tape, etc.)

	FSTT_OPTBUFSIZE		Minimum optimal buffer size for efficient
				sequential i/o.  Actual buffer size may be
				any integer multiple of this value.

	FSTT_MAXBUFSIZE		Maximum buffer size permitted, i.e., maximum
				size transfer permitted in a call to aread
				or awrite.  FIO will not create a buffer
				larger than this value, but will try to use
				a larger buffer if created externally.

	FSTT_FILSIZE		File size, chars.  This is requested only
				once for an open file, and is not requested
				for streaming binary files.
.endhelp ____________________________________________________________________

procedure fgdev_param (fd)

int	fd
pointer	ffp
long	fgdev0(), ffilsz()
errchk	fgdev0, ffilsz
include	<fio.com>

begin
	ffp = fiodes[fd]

	FBLKSIZE(ffp)    = max (0, fgdev0 (ffp, FSTT_BLKSIZE))
	FOPTBUFSIZE(ffp) = max (1, fgdev0 (ffp, FSTT_OPTBUFSIZE))
	FMAXBUFSIZE(ffp) = max (0, fgdev0 (ffp, FSTT_MAXBUFSIZE))
	FIRSTBUFOFF(ffp) = 1

	# If regular device, and file size is not yet known, get file size.
	if (FBLKSIZE(ffp) > 0 && FILSIZE(ffp) < 0) {
	    FILSIZE(ffp) = fgdev0 (ffp, FSTT_FILSIZE)
	    FILSIZE(ffp) = ffilsz (fd)			# add buffered output
	}

	if (FTYPE(ffp) == BINARY_FILE)
	    FBUFSIZE(ffp) = FOPTBUFSIZE(ffp)
	else
	    FBUFSIZE(ffp) = max (SZ_LINE, FOPTBUFSIZE(ffp))
end


# FGDEV0 -- Internal procedure to get status from either a text or binary
# file, rounding the byte count up to an integral number of chars.

long procedure fgdev0 (ffp, what)

pointer	ffp
int	what

long	nbytes
int	status_epa
include	<fio.com>

begin
	if (FTYPE(ffp) == BINARY_FILE)
	    status_epa = ZSTTBF(ffp)
	else
	    status_epa = ZSTTTX(ffp)

	call zcall3 (status_epa, FCHAN(ffp), what, nbytes)
	if (nbytes == ERR)
	    call filerr (FNAME(ffp), SYS_FDEVSTAT)

	if (FTYPE(ffp) == BINARY_FILE)
	    return ((nbytes+SZB_CHAR-1) / SZB_CHAR)
	else
	    return (nbytes)
end
