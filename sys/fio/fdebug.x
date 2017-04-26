# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<fio.h>

# FDEBUG -- Decode and print the contents of a file descriptor or of all
# file descriptors on the standard output.

procedure fdebug (out, fd1_arg, fd2_arg)

int	out, fd1_arg, fd2_arg
int	fd, fd1, fd2, n
int	and()
long	note()
pointer	ffp
include	<fio.com>

begin
	fd1 = max(1, min(LAST_FD, fd1_arg))
	if (fd2_arg <= 0)
	    fd2 = LAST_FD
	else
	    fd2 = max(1, min(LAST_FD, fd2_arg))

	if (fd1 < FIRST_FD) {
	    n = 0					# count open files
	    do fd = 1, LAST_FD
		if (fiodes[fd] != NULL)
		    n = n + 1

	    call fprintf (out,
		"FIO Status: %d open files, %d installed devices\n\n")
		call pargi (n)
		call pargi (next_dev / LEN_DTE)		# count devices
	}

	for (fd=fd1;  fd <= fd2;  fd=fd+1) {
	    ffp = fiodes[fd]
	    if (ffp != NULL) {
		call fprintf (out, "%2d  (%s), %s, %s, fp=%d,\n")
		    call pargi (fd)
		    call pargstr (FNAME(ffp))

		    switch (FMODE(ffp)) {
		    case READ_ONLY:
			call pargstr ("READ_ONLY")
		    case READ_WRITE:
			call pargstr ("READ_WRITE")
		    case WRITE_ONLY:
			call pargstr ("WRITE_ONLY")
		    case APPEND:
			call pargstr ("APPEND")
		    case NEW_FILE:
			call pargstr ("NEW_FILE")
		    case TEMP_FILE:
			call pargstr ("TEMP_FILE")
		    default:
			call pargstr ("ILLEGAL_FMODE")
		    }

		    switch (FTYPE(ffp)) {
		    case TEXT_FILE:
			call pargstr ("TEXT_FILE")
		    case BINARY_FILE:
			call pargstr ("BINARY_FILE")
		    case STRING_FILE:
			call pargstr ("STRING_FILE")
		    case SPOOL_FILE:
			call pargstr ("SPOOL_FILE")
		    default:
			call pargstr ("ILLEGAL_FTYPE")
		    }
		    call pargi (ffp)

		call fprintf (out, "    ")
		call fprintf (out,
		"chan=%d, device=%d, epa=0%xX, filesize(chars)=%d, posn=%s,\n")
		    call pargi (FCHAN(ffp))
		    call pargi ((FDEV(ffp)-1) / LEN_DTE + 1)
		    call pargi (zdev[FDEV(ffp)])
		    call pargl (FILSIZE(ffp))

		    if (FILSIZE(ffp) < 0)
			call pargl (note(fd))
		    else if (boffset[fd] > FILSIZE(ffp))
			call pargstr ("EOF")
		    else
			call pargl (note(fd))

		call fprintf (out, "    ")
		call fprintf (out,
		"iomode=%s, status=%s, refcnt=%d, afd=%d,\n")
		    switch (FFIOMODE(ffp)) {
		    case INACTIVE:
			call pargstr ("INACTIVE")
		    case READ_IN_PROGRESS:
			call pargstr ("READ_IN_PROGRESS")
		    case WRITE_IN_PROGRESS:
			call pargstr ("WRITE_IN_PROGRESS")
		    default:
			call pargstr ("ILLEGAL")
		    }

		    switch (FILSTAT(ffp)) {
		    case ERR:
			call pargstr ("ERR")
		    case OK:
			call pargstr ("OK")
		    default:
			call pargi (FILSTAT(ffp))
		    }

		    call pargi (FREFCNT(ffp))
		    call pargi (FAFD(ffp))

		call fprintf (out, "    ")
		call fprintf (out,
		"nbufs=%d, bufsize=%d, optbufsize=%d, blksize=%d,\n")
		    call pargi (FNBUFS(ffp))
		    call pargi (FBUFSIZE(ffp))
		    call pargi (FOPTBUFSIZE(ffp))
		    call pargi (FBLKSIZE(ffp))

		call fprintf (out, "    ")
		call fprintf (out,
		"pbbufsize=%d, pbbuf=%d, pbtop=%d, pbiop=%d, pbsp=%d,\n")
		    call pargi (FPBBUFSIZE(ffp))
		    call pargi (FPBBUF(ffp))
		    call pargi (FPBTOP(ffp))
		    call pargi (FPBIOP(ffp))
		    call pargi (FPBSP(ffp))

		call fprintf (out, "    ")
		call fprintf (out,
		"iop=%d, itop=%d, otop=%d, bp=%d, top=%d, offset=%d,\n")
		    call pargi (iop[fd])
		    call pargi (itop[fd])
		    call pargi (otop[fd])
		    call pargi (bufptr[fd])
		    call pargi (buftop[fd])
		    call pargi (boffset[fd])

		call fprintf (out, "    Flags =")
		if (and (FF_FLUSH, fflags[fd]) != 0)
		    call fprintf (out, " FLUSH")
		if (and (FF_FLUSHNL, fflags[fd]) != 0)
		    call fprintf (out, " FLUSHNL")
		if (and (FF_READ, fflags[fd]) != 0)
		    call fprintf (out, " READ")
		if (and (FF_WRITE, fflags[fd]) != 0)
		    call fprintf (out, " WRITE")
		if (and (FF_KEEP, fflags[fd]) != 0)
		    call fprintf (out, " KEEP")
		if (and (FF_EOF, fflags[fd]) != 0)
		    call fprintf (out, " EOF")
		if (and (FF_ERR, fflags[fd]) != 0)
		    call fprintf (out, " ERR")
		if (and (FF_PUSHBACK, fflags[fd]) != 0)
		    call fprintf (out, " PUSHBACK")
		call fprintf (out, "\n\n")
	    }
	}
end
