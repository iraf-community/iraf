# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<fio.h>

# FFILBF -- Called by FFAULT to fill the file buffer for a binary file.

procedure ffilbf (fd, bp, bufsize, buffer_offset)

int	fd, bufsize
pointer	bp
bool	at_eof, stream_dev
long	buffer_offset
errchk	fwatio
include	<fio.com>

begin
	fp = fiodes[fd]

	if (FBUFMODE(fp) != INACTIVE)
	    call fwatio (fd)

	# If streaming device, read unconditionally, otherwise if
	# positioned at EOF, initialize buffer pointers and return,
	# else initiate read to fill buffer from file and return.

	stream_dev = (FBLKSIZE(fp) == 0)
	at_eof = (FILSIZE(fp) >= 0 && buffer_offset > FILSIZE(fp))

	if (!stream_dev && at_eof) {
	    itop[fd] = bufptr[fd]
	    otop[fd] = bufptr[fd]
	} else {
	    call aread (fd, Memc[bp], bufsize, buffer_offset)
	    FBUFMODE(fp) = READ_IN_PROGRESS
	}
end
