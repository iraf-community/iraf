# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<syserr.h>
include	<fio.h>

# UNREAD -- Push a binary block of data back into the input stream.  Pushback
# is last in first out, i.e., the pushed back data will be read before input
# resumes at the point at which it was interrupted.  Multiple blocks of data
# may be pushed back into the input until the push back buffer overflows.
# Overflow is often an indication of recursion in whatever routine is doing
# the pushback.

procedure unread (fd, buf, nchars)

int	fd			# file
char	buf[ARB]		# data block to be pushed back
int	nchars			# nchars to push back

int	or()
pointer	pb_iop, pb_sp
errchk	syserr, syserrs, fmkpbbuf
include	<fio.com>

begin
	fp = fiodes (fd)
	if (fd <= 0 || fp == NULL)
	    call syserr (SYS_FILENOTOPEN)

	if (FPBBUF(fp) == NULL)
	    call fmkpbbuf (fd)

	# Push the old pb_iop, iop, itop and bufptr on the stack for later
	# restoration of the interrupted input stream by filbuf.  Note bufptr
	# must be set to <= iop to avoid a buffer fault.

	pb_sp  = FPBSP(fp) - 1
	pb_iop = FPBIOP(fp)

	Memi[pb_sp] = pb_iop
	pb_sp = pb_sp - 1
	Memi[pb_sp] = bufptr[fd]
	pb_sp = pb_sp - 1
	Memi[pb_sp] = itop[fd]
	pb_sp = pb_sp - 1
	Memi[pb_sp] = iop[fd]

	# Check that room remains for the data.
	if (((pb_sp - 1) * SZ_INT + 1) - pb_iop < nchars)
	    call syserrs (SYS_FPBOVFL, FNAME(fp))

	# Move the data block into the buffer.  Set the iop to point to the
	# first char of the block.  Note: data grows upwards while the stack
	# grows downward.

	bufptr[fd] = pb_iop
	iop[fd] = pb_iop
	call amovc (buf, Memc[pb_iop], nchars)
	pb_iop = pb_iop + nchars
	itop[fd] = pb_iop
	
	FPBSP(fp) = pb_sp
	FPBIOP(fp) = pb_iop
	fflags[fd] = or (fflags[fd], FF_PUSHBACK)
end
