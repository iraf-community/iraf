include "../lib/display.h"

# AP_PPARS -- Procedure to write out the phot task parameters.

procedure ap_ppars (ap)

pointer	ap		# pointer to apphot structure

bool	itob()
int	apstati()

begin
	# Write the data dependent parameters.
	call ap_dapars (ap)

	# Write the centering  parameters.
	call ap_cepars (ap)

	# Write the sky fitting paameters.
	call ap_sapars (ap)

	# Write the photometry parameters.
	call ap_phpars (ap)

	call clputb ("radplots", itob (apstati (ap, RADPLOTS)))
end
