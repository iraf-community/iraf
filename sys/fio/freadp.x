# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <config.h>
include <syserr.h>
include <fio.h>

# FREADP -- Read from a file, directly accessing the file data in the FIO
# buffer rather than copying the data from the FIO buffer to the user buffer.
# This technique can be used for very efficient file access, but is not as
# general as an ordinary read.  In particular the requested data segment
# must lie entirely within the FIO buffer, and the referenced data must be
# used before a file fault causes the buffer contents to be replaced.  The
# file size should be known in advance and any attempt to read outside the
# file boundaries is interpreted as an error.

pointer procedure freadp (fd, offset, nchars)

int	fd		# file to be accessed
long	offset		# file offset in chars
int	nchars		# nchars to "read"

pointer	bp, fiop
int	ffault()
errchk	filerr, ffault, fmkbfs
include	<fio.com>

begin
	# Move file buffer onto file block containing the file offset.
	# Verify that the buffer contains nchars of file data in contiguous
	# storage.  If the file buffer already contains the referenced
	# data segment no fault is necessary and this is quite fast.
	# The iop is left pointing to the first char following the
	# referenced data block.

	repeat {
	    bp   = bufptr[fd]
	    fiop = offset - boffset[fd] + bp		# lseek

	    if (fiop < bp || fiop >= itop[fd]) {
		if (bp == NULL) {
		    call fmkbfs (fd)
		    next
		}
		if (ffault (fd, offset, nchars, FF_READ) == EOF)
		    call filerr (FNAME(fiodes[fd]), SYS_FREADP)
		fiop = iop[fd]
	    }

	    iop[fd] = fiop + nchars
	    if (iop[fd] > itop[fd])
		call filerr (FNAME(fiodes[fd]), SYS_FREADP)

	    return (fiop)
	}
end
