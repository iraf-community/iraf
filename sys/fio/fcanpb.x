# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<fio.h>

# FCANPB -- Cancel any pushed back data, restoring the original file pointers.
# This should be done before performing any non-i/o operation which relys
# upon the FIO data structures being in their normal state (e.g., before the
# file buffers are deallocated).

procedure fcanpb (fd)

int	fd		# file descriptor

int	pb_sp
int	and()
include	<fio.com>

begin
	fp = fiodes[fd]

	while (and (fflags[fd], FF_PUSHBACK) != 0) {
	    pb_sp = FPBSP(fp)

	    iop[fd]	= Memi[pb_sp];	pb_sp = pb_sp + 1
	    itop[fd]	= Memi[pb_sp];	pb_sp = pb_sp + 1
	    bufptr[fd]	= Memi[pb_sp];	pb_sp = pb_sp + 1
	    FPBIOP(fp)	= Memi[pb_sp];	pb_sp = pb_sp + 1

	    FPBSP(fp) = pb_sp

	    # When the pb stack pointer reaches the top of the pushback buffer,
	    # all pushed back data has been read.  Note that the stack pointer
	    # is a pointer to int while FPBTOP is a pointer to char.

	    if (pb_sp >= (FPBTOP(fp) - 1) / SZ_INT + 1)
		fflags[fd] = fflags[fd] - FF_PUSHBACK
	}
end
