# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<fio.h>

# FMKPBBUF -- Make the push back buffer.  Called when the first attempt is
# made to push data back into an input stream.

procedure fmkpbbuf (fd)

int	fd
int	buflen
pointer	bp
errchk	malloc
include	<fio.com>

begin
	fp = fiodes[fd]
	if (bufptr[fd] == NULL)
	    call fmkbfs (fd)

	buflen = FPBBUFSIZE(fp)
	if (buflen <= 0) {
	    buflen = SZ_PBBUF
	    FPBBUFSIZE(fp) = buflen
	}

	call malloc (bp, buflen, TY_CHAR)

	FPBBUF(fp) = bp
	FPBTOP(fp) = bp + buflen
	FPBIOP(fp) = bp
	FPBSP(fp) = (FPBTOP(fp) - 1) / SZ_INT + 1
end
