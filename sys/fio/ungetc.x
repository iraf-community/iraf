# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<syserr.h>
include	<fio.h>

# UNGETC -- Push a character back into the input stream.  Pushback is last
# in first out, i.e., the last character pushed will be the first character
# returned in the next getc, getline, or read.  Multiple characters and
# strings may be pushed back into the input until the push back buffer
# overflows.  Overflow is often an indication of recursion in whatever
# routine is doing the pushback.
#
# Single character pushback is fairly expensive, but the i/o system is
# very efficient for even character at a time input and is optimized with
# that in mind.  The overhead for pushing back an entire string is about
# the same as for a single character, so recursive macro expansion may be
# implemented quite efficiently with this pushback technique.

procedure ungetc (fd, ch)

int	fd			# file
char	ch			# char to be pushed back
pointer	pb_sp, pb_iop
int	or()
include	<fio.com>

begin
	fp = fiodes[fd]
	if (fd <= 0 || fp == NULL)
	    call syserr (SYS_FILENOTOPEN)

	if (FPBBUF(fp) == NULL)
	    call fmkpbbuf (fd)

	# Push the old pb_iop, iop, itop and bufptr on the stack.  Note bufptr
	# must be set to a value less than or equal to iop to avoid a buffer
	# fault.

	pb_iop = FPBIOP(fp)
	pb_sp  = FPBSP(fp) - 1

	Memi[pb_sp] = pb_iop
	pb_sp = pb_sp - 1
	Memi[pb_sp] = bufptr[fd]
	pb_sp = pb_sp - 1
	Memi[pb_sp] = itop[fd]
	pb_sp = pb_sp - 1
	Memi[pb_sp] = iop[fd]

	# Deposit the char in the pbbuf and set up i/o pointers.  When iop
	# reaches itop filbuf will pop the old input pointers off the pbstack.
	# Note: pushed back data grows upward, while the pb stack grows
	# downward.

	Memc[pb_iop] = ch
	bufptr[fd] = pb_iop
	iop[fd] = pb_iop
	pb_iop = pb_iop + 1
	itop[fd] = pb_iop

	# Check for overflow.
	if (pb_iop >= (pb_sp - 1) * SZ_INT + 1)
	    call syserrs (SYS_FPBOVFL, FNAME(fp))
	
	FPBSP(fp) = pb_sp
	FPBIOP(fp) = pb_iop
	fflags[fd] = or (fflags[fd], FF_PUSHBACK)
end
