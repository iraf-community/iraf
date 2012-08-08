# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<fio.h>

# FRMBFS -- Return file buffer.  Called by FSET to change the buffer size,
# and by FRTNFD when a file is closed.

procedure frmbfs (fd)

int	fd			# file descriptor

long	offset
errchk	mfree, flush
include	<fio.com>

begin
	fp = fiodes[fd]
	if (bufptr[fd] == NULL)
	    return
	else
	    call fcanpb (fd)

	# Note file offset, return buffer and initialize i/o pointers,
	# restore seek offset (which depends on buffer pointer, buf offset).

	offset = LNOTE(fd)
	call mfree (bufptr[fd], TY_CHAR)
	call mfree (FPBBUF(fp), TY_CHAR)

	bufptr[fd] = NULL
	boffset[fd] = NULL
	buftop[fd] = NULL
	itop[fd] = NULL
	otop[fd] = NULL

	LSEEK (fd, offset)
end
