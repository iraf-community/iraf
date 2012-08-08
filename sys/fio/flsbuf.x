# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<syserr.h>
include	<fio.h>

# FLSBUF -- Flush the file buffer.  Called by PUTC, PUTLINE, and WRITE
# when the i/o pointer no longer points into the file buffer.  Prior to
# the first write to a buffer, the OTOP pointer will be set to the 
# beginning of the buffer.  The first call to FLSBUF advances OTOP to the
# end of the buffer.  The next call to FLSBUF finds the buffer "dirty",
# and flushes the buffer, leaving the buffer ready to be written into
# (OTOP left pointing at the end of the buffer).  A seek on a binary file
# will usually leave the i/o pointer pointing outside the buffer, which
# requires a call to FFAULT (file fault).

procedure flsbuf (fd, nreserve)

int	fd, nreserve
pointer	bp
bool	iop_in_range
int	nchars_written, ffault(), and()
errchk	fmkbfs, syserr, filerr, ffault
include	<fio.com>

begin
	fp = fiodes[fd]
	bp = bufptr[fd]

	if (fd <= 0 || fp == NULL)			# verification
	    call syserr (SYS_FILENOTOPEN)
	else if (and (FF_WRITE, fflags[fd]) == 0) {
	    if (FTYPE(fp) == SPOOL_FILE) {
		if (otop[fd] < buftop[fd])
		    otop[fd] = buftop[fd]
		else
		    call fexbuf (fd)
		return
	    } else
		call filerr (FNAME(fp), SYS_FNOWRITEPERM)
	}

	iop_in_range = iop[fd] >= bufptr[fd] && iop[fd] < buftop[fd]

	if (bp == NULL) {				# no buffer yet
	    call fmkbfs (fd)
	    bp = bufptr[fd]
	    itop[fd] = bp
	    if (FTYPE(fp) == BINARY_FILE)
		nchars_written = ffault (fd, LNOTE(fd), nreserve, FF_WRITE)
	    else
		nchars_written = 0
					
	} else if (iop_in_range && otop[fd] < buftop[fd]) {
	    nchars_written = 0				# buffer not full yet?

	} else if (FTYPE(fp) == TEXT_FILE) {		# text files
	    call fputtx (fd, Memc[bp], iop[fd] - bp, nchars_written)
	    iop[fd] = bp
	    itop[fd] = bp

	} else						# binary files
	    nchars_written = ffault (fd, LNOTE(fd), nreserve, FF_WRITE)

	otop[fd] = buftop[fd]				# make space available

	if (nchars_written == ERR)
	    call filerr (FNAME(fp), SYS_FWRITE)
end
