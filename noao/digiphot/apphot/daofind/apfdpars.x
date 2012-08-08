include "../lib/display.h"

# AP_FDPARS -- Write out the current daofind parameters to the current
# parameter files.

procedure ap_fdpars (ap)

pointer	ap		# pointer to apphot structure

begin
	# Write the data dependent parameters.
	call ap_dapars (ap)

	# Write the daofind parameters.
	call ap_fipars (ap)
end
