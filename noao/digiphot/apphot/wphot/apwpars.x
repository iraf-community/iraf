include "../lib/display.h"

# AP_WPARS -- Procedure to write out the wphot task parameters.

procedure ap_wpars (ap)

pointer	ap		# pointer to apphot structure

bool	itob()
int	apstati()

begin
	# Write out the data dependent parameters.
	call ap_dapars (ap)

	# Write the centering parameters.
	call ap_cepars (ap)

	# Write out the sky fitting parameters.
	call ap_sapars (ap)

	# Write out the photometry parameters.
	call ap_phpars (ap)

	# Set the radplots parameter.
	call clputb ("radplots", itob (apstati (ap, RADPLOTS)))
end
