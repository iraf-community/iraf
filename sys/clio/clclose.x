# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>

# CLCLOSE -- "Close" the CL files (shut down CLIO).  Called by the IRAF Main
# upon process shutdown.

procedure clclose ()

int	fd

begin
	# Remove buffers for the standard streams.
	do fd = 1, FIRST_FD-1
	    call frmbfs (fd)
end
