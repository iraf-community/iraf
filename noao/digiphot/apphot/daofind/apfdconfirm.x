include "../lib/apphot.h"
include "../lib/noise.h"

# AP_FDCONFIRM -- Procedure to confirm the critical daofind parameters.

procedure ap_fdconfirm (ap)

pointer	ap		# pointer to the apphot structure

real	rval
real	ap_vfwhmpsf(), ap_vthreshold()

begin
	call printf ("\n")

	# Verify the critical parameters.
	rval = ap_vfwhmpsf (ap)
	rval  = ap_vthreshold (ap)

	call printf ("\n")
end
