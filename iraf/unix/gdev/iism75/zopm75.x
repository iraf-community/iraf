# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	"m75.h"

# ZOPM75 -- Open the IIS for binary file i/o.

procedure zopm75 (device, mode, ifcb)

char	device[ARB]		# packed UNIX device name
int	mode			# access mode
int	ifcb			# pointer to channel descriptor passed as int

size_t	sz_val
pointer	fcb
int	chan

begin
	sz_val = LEN_FCB
	call calloc (fcb, sz_val, TY_STRUCT)
	ifcb = fcb

	FCB_STATUS(fcb)   = IIS_INACTIVE
	FCB_NBYTES(fcb)   = 0
	FCB_STATE(fcb)    = READY

	call zopnbf (device, mode, chan)

	if (chan < 0) {
	    call mfree (fcb, TY_STRUCT)
	    ifcb = ERR
	} else
	    FCB_CHAN(fcb) = chan
end
