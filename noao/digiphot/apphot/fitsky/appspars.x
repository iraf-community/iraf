include "../lib/display.h"

# AP_PSPARS -- Procedure to write out the current sky fitting parameters
# to the parameter files.

procedure ap_pspars (ap)

pointer	ap		# pointer to apphot structure

bool	itob()
int	apstati()

begin
	# Write the data dependent parameters.
	call ap_dapars (ap)

	# Write the sky fitting parameters.
	call ap_sapars (ap)

	# Radial profile plots
	call clputb ("radplots", itob (apstati (ap, RADPLOTS)))
end
