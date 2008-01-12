include "../lib/noise.h"
include "../lib/display.h"

# AP_FDGPARS -- Open up the apphot data structure and get the daofind input
# parameters.

procedure ap_fdgpars (ap)

pointer	ap			# pointer to the apphot structure

begin
	# Open the apphot structure.
	call ap_fdinit (ap, 2.0, AP_NPOISSON)

	# Get the data dependent parameters.
	call ap_gdapars (ap)

	# Get the find parameters.
	call ap_gfipars (ap)
end
