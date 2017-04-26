# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<qpioset.h>
include	"qpio.h"

# QPIO_STATR -- Stat a QPIO interface real valued parameter.

real procedure qpio_statr (io, param)

pointer	io			#I QPIO descriptor
int	param			#I parameter code

bool	fp_equalr()

begin
	switch (param) {
	case QPIO_BLOCKFACTOR:
	    if (fp_equalr (IO_XBLOCK(io), IO_YBLOCK(io)))
		return (IO_XBLOCK(io))
	    else
		return (ERR)
	case QPIO_XBLOCKFACTOR:
	    return (IO_XBLOCK(io))
	case QPIO_YBLOCKFACTOR:
	    return (IO_YBLOCK(io))
	}

	return (ERR)
end
