# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<syserr.h>
include	<fio.h>

# UNGETLINE -- Push an EOS delimited string back into the input stream.
# The next getc will return the first char of the pushed back string,
# followed by successive chars in the string until EOS is reached, at which
# time input reverts to wherever it was before pushback.  Pushback is last
# in first out, i.e., the last string pushed will be the first string scanned
# when reading.  Multiple characters and strings may be pushed back into the
# input until the push back buffer overflows.  Overflow is often an indication
# of recursion in whatever routine is doing the pushback.
#
# N.B.: this routine really pushes a string, not a line, i.e. we don't give
# a whiz about newline characters.

procedure ungetline (fd, str)

int	fd			# file
char	str[ARB]		# string to be pushed back

int	or()
pointer	ip, pb_iop, pb_sp, iop_limit
errchk	syserr, syserrs, fmkpbbuf
include	<fio.com>

begin
	fp = fiodes[fd]
	if (fd <= 0 || fp == NULL)
	    call syserr (SYS_FILENOTOPEN)

	if (str[1] == EOS)
	    return
	if (FPBBUF(fp) == NULL)
	    call fmkpbbuf (fd)

	# Push the old pb_iop, iop, itop and bufptr on the stack for later
	# restoration of the interrupted input stream by filbuf.  Note bufptr
	# must be changed to point to a value less than iop to avoid a buffer
	# fault.

	pb_sp  = FPBSP(fp) - 1
	pb_iop = FPBIOP(fp)

	Memi[pb_sp] = pb_iop
	pb_sp = pb_sp - 1
	Memi[pb_sp] = bufptr[fd]
	pb_sp = pb_sp - 1
	Memi[pb_sp] = itop[fd]
	pb_sp = pb_sp - 1
	Memi[pb_sp] = iop[fd]

	# Copy the string into the buffer; abort if the buffer overflows.
	# Set iop to point to first char of string.  Note: pushed back chars
	# grow upward while the stacked i/o pointers grow downward.

	bufptr[fd] = pb_iop
	iop[fd] = pb_iop
	iop_limit = (pb_sp - 1) * SZ_INT + 1

	for (ip=1;  str[ip] != EOS;  ip=ip+1) {
	    if (pb_iop >= iop_limit)
		call syserrs (SYS_FPBOVFL, FNAME(fp))
	    Memc[pb_iop] = str[ip]
	    pb_iop = pb_iop + 1
	}

	itop[fd] = pb_iop
	
	FPBSP(fp) = pb_sp
	FPBIOP(fp) = pb_iop
	fflags[fd] = or (fflags[fd], FF_PUSHBACK)
end
