# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<config.h>
include	<fio.h>

# FILBUF -- Fill the file buffer.  Called by GETC, GETLINE, and READ when the
# i/o pointer no longer points into the file buffer.  This happens when
# (1) there is no file buffer yet, (2) all the data in the buffer has been
# read, or (3) a SEEK has occurred.

int procedure filbuf (fd)

int	fd			#I input file

pointer	bp, pb_sp
int	maxch, nchars_read

int	ffault()
errchk	fmkbfs, ffault, filerr, syserr
include	<fio.com>
define	again_ 91

begin
	fp = fiodes[fd]
	if (fd <= 0 || fp == NULL)			# verification
	    call syserr (SYS_FILENOTOPEN)
again_
	if (and (FF_READ+FF_PUSHBACK, fflags[fd]) == 0) {
	    if (FTYPE(fp) == STRING_FILE || FTYPE(fp) == SPOOL_FILE)
		return (EOF)
	    else
		call filerr (FNAME(fp), SYS_FNOREADPERM)
	}

	# If filbuf was called at the end of a pushed back block of data,
	# pop the old i/o pointers off the pushback stack and resume i/o
	# at the point at which it was interrupted.

	if (and (fflags[fd], FF_PUSHBACK) != 0) {
	    repeat {
		pb_sp = FPBSP(fp)

		iop[fd]    = Memi[pb_sp];	pb_sp = pb_sp + 1
		itop[fd]   = Memi[pb_sp];	pb_sp = pb_sp + 1
		bufptr[fd] = Memi[pb_sp];	pb_sp = pb_sp + 1
		FPBIOP(fp) = Memi[pb_sp];	pb_sp = pb_sp + 1

		FPBSP(fp) = pb_sp

		# When the pb stack pointer reaches the top of the pushback
		# buffer, all pushed back data has been read.  Note that the
		# stack pointer is a pointer to int while FPBTOP is a pointer
		# to char.

		if (pb_sp >= (FPBTOP(fp) - 1) / SZ_INT + 1)
		    fflags[fd] = fflags[fd] - FF_PUSHBACK

		# If there was no data left when pushback occurred, then we
		# aren't done yet.

		nchars_read = itop[fd] - iop[fd]
		if (nchars_read > 0)
		    return (nchars_read)

	    } until (and (fflags[fd], FF_PUSHBACK) == 0)
	    goto again_
	}

	# If we do not have a file buffer yet, allocate one.
	bp = bufptr[fd]
	if (bp == NULL) {
	    call fmkbfs (fd)
	    bp = bufptr[fd]
	}

	if (FTYPE(fp) == TEXT_FILE) {
	    # Get next line from text file, initialize pointers.  In raw mode
	    # we only read one character at a time.

	    if (and (FF_RAW, fflags[fd]) == 0)
		maxch = FBUFSIZE(fp)
	    else
		maxch = 1
	    call zcall4 (ZGETTX(fp), FCHAN(fp), Memc[bp], maxch, nchars_read)

	    iop[fd] = bp
	    itop[fd] = max (bp, bp + nchars_read)
	    otop[fd] = bp

	} else if (FNCHARS(fp) < 0) {
	    # Validate data in buffer without performing a physical read (used
	    # to attempt error recovery following a read error - see fseti).

	    nchars_read = -FNCHARS(fp)
	    iop[fd] = bp
	    itop[fd] = bp + nchars_read
	    otop[fd] = bp

	} else {
	    # Fill buffer from binary file.
	    nchars_read = ffault (fd, LNOTE(fd), 0, FF_READ)
	}

	switch (nchars_read) {
	case ERR:
	    call filerr (FNAME(fp), SYS_FREAD)
	case 0:
	    return (EOF)
	default:
	    return (nchars_read)			# (or ERR)
	}
end
