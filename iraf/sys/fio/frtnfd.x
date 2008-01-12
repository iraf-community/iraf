# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<fio.h>

# FRTNFD -- Return file descriptor and buffers.

procedure frtnfd (fd)

int	fd
include	<fio.com>

begin
	if (fiodes[fd] != NULL) {
	    call frmbfs (fd)
	    call mfree (fiodes[fd], TY_STRUCT)
	    fiodes[fd] = NULL
	}
end
