# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <config.h>
include <syserr.h>
include <fio.h>

# FWRITEP -- Write to a file, directly accessing the file data in the FIO
# buffer rather than copying the data from the user buffer to the FIO buffer.
# This technique can be used for very efficient file access, but is not as
# general as an ordinary write.  In particular the requested data segment
# must lie entirely within the FIO buffer, and the data must be written into
# the FIO buffer before a file fault causes the buffer contents to be flushed
# to disk.  The file size should be known in advance and any attempt to write
# outside the file boundaries is interpreted as an error.
#
# NOTE -- This routine returns a pointer into the FIO buffer.  No data is
# transferred in the call itself.  The data is not actually written to the
# output file until the FIO buffer is faulted out.  If the output file is
# readwrite and offset,nchars does not span the entire buffer, file data
# will be read into the buffer when it is first faulted in.  Hence, FWRITEP
# may be used for updating the contents of a file.

pointer procedure fwritep (fd, offset, nchars)

int	fd		# file to be accessed
long	offset		# file offset in chars
int	nchars		# nchars to "write"

int	junk
pointer	fiop, bp
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

	    if (fiop < bp || fiop >= otop[fd]) {
		if (bp == NULL) {
		    call fmkbfs (fd)
		    next
		}
		junk = ffault (fd, offset, nchars, FF_WRITE)
		fiop = iop[fd]
		otop[fd] = buftop[fd]
	    }

	    iop[fd] = fiop + nchars
	    if (iop[fd] > otop[fd])
		call filerr (FNAME(fiodes[fd]), SYS_FWRITEP)

	    return (fiop)
	}
end
