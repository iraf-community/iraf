include "../lib/display.h"

# AP_PCPARS -- Procedure to write out the current centering parameters
# to the current parameter files.

procedure ap_pcpars (ap)

pointer	ap		# pointer to apphot structure
bool	itob()
int	apstati()

begin
	# Write the data dependent parameters.
	call ap_dapars (ap)

	# Write the centering parameters.
	call ap_cepars (ap)

	# Set the plotting command.
	call clputb ("radplots", itob (apstati (ap, RADPLOTS)))
end
