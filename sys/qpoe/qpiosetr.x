# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<qpioset.h>
include	"qpio.h"

# QPIO_SETR -- Set a QPIO interface real valued parameter.  This procedure
# represents the lowest level interface by which an applications program can
# control QPIO.

procedure qpio_setr (io, param, value)

pointer	io			#I QPIO descriptor
int	param			#I parameter code
real	value			#I new parameter value

begin
	# Almost everything here cancels any active i/o.
	IO_ACTIVE(io) = NO

	# Set the named parameter.
	switch (param) {
	case QPIO_BLOCKFACTOR:
	    IO_XBLOCK(io) = value
	    IO_YBLOCK(io) = value
	case QPIO_XBLOCKFACTOR:
	    IO_XBLOCK(io) = value
	case QPIO_YBLOCKFACTOR:
	    IO_YBLOCK(io) = value
	}
end
