# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<config.h>
include	<fio.h>

# FFLSBF -- Flush the file buffer.  Called by FFAULT to initiate a write
# of the file buffer when a fault occurs.

procedure fflsbf (fd, bp, maxchars, buffer_offset)

int	fd
pointer	bp
int	maxchars
long	buffer_offset
errchk	fwatio
include	<fio.com>

begin
	fp = fiodes[fd]

	if (FBUFMODE(fp) != INACTIVE)
	    call fwatio (fd)
	call awrite (fd, Memc[bp], maxchars, buffer_offset)

	FBUFMODE(fp) = WRITE_IN_PROGRESS
end
