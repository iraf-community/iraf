# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<error.h>
include	<config.h>
include	<fio.h>

# FFAULT -- Read a file block into the file buffer (pick up file buffer and
# put it down on another part of the file).  Although in this implementation
# there is only a single, local, file buffer for each file, in the future
# a variable number of either global or local buffers will be supported, as
# well as read ahead and write behind (see Fio.doc).

int procedure ffault (fd, file_offset, nreserve, rwflag)

int	fd
long	file_offset		# char offset to be faulted in
int	nreserve		# size of transfer pending
int	rwflag			# next access is a read or a write

pointer	bp
long	buffer_offset, fboff
int	bufsize, nchars_read
bool	block_write, stream, at_eof

int	and()
errchk	ffilbf, fflsbf, fwatio
define	ioerror_ 91
include	<fio.com>

begin
	# assert (file open, buffer already created)
	# assert (iop does not point into buffer)

	fp = fiodes[fd]
	bp = bufptr[fd]
	bufsize = FBUFSIZE(fp)
	fboff   = FIRSTBUFOFF(fp)
	stream  = (FBLKSIZE(fp) == 0)

	# Calculate buffer_offset (modulus file buffer size).  If the output
	# device is a pipe or terminal (stream device), which does not permit
	# rewriting of data and seeking, empty buffer.

	if (stream) {
	    buffer_offset = file_offset	
	} else if (file_offset <= 0) {
	    iferr (call filerr (FNAME(fp), SYS_FSEEK))
		goto ioerror_
	} else
	    buffer_offset = (file_offset-fboff) / bufsize * bufsize + fboff

	# Update i/o pointers (if an empty or partially full buffer has been
	# written into, determine the top of the valid part of the buffer).

	UPDATE_IOP(fd)
	    
	# Flush buffer if it has been written into.  Write out only as much
	# of the buffer as has been filled.

	if (BUF_MODIFIED(fd)) {
	    iferr (call fflsbf (fd, bp, otop[fd]-bp, boffset[fd]))
		goto ioerror_

	    # We need to do this wait here since we immediately use this buffer
	    # without doing a wait. Which screws up the data going out to disk.
	    # This can be done away with when multi-buffering is done. (FJR).

	    call fwatio (fd)
	}

	# Fill buffer from file only if the file was opened with read
	# permission, and if the fault was not caused by a WRITE which will
	# immediately overwrite the entire contents of the buffer.

	if (rwflag == FF_WRITE) {
	    block_write = (stream ||
		(file_offset == buffer_offset && nreserve >= bufsize))
	} else
	    block_write = false

	if (block_write) {
	    itop[fd] = bp
	    otop[fd] = bp

	} else if (and(FF_READ,fflags[fd]) == 0) {
	    # Read is disabled.  Zero-fill buffer; if inside existing
	    # random access file, set ITOP to end of buffer so that the
	    # entire buffer will be written when flushed.

	    at_eof = (FILSIZE(fp) >= 0 && buffer_offset > FILSIZE(fp))
	    otop[fd] = bp

	    if (at_eof)
		itop[fd] = bp
	    else
		itop[fd] = bp + bufsize

	    # Zero-fill the buffer.
	    call aclrc (Memc[bp], bufsize)

	} else {
	    iferr {
		# Initialize buffer from file.
		call ffilbf (fd, bp, bufsize, buffer_offset)
		call fwatio (fd)
	    } then
		goto ioerror_
	}

	boffset[fd] = buffer_offset
	LSEEK (fd, file_offset)				# set i/o pointer

	nchars_read = itop[fd] - iop[fd]
	if (nchars_read <= 0)
	    return (EOF)
	else
	    return (nchars_read)			# only valid for a read

	# If an i/o error occurs, mark the buffer empty and pass the error
	# back to our caller.

ioerror_
	itop[fd] = bp
	otop[fd] = bp
	call erract (EA_ERROR)
end
