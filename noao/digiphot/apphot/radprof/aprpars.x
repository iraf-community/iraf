include "../lib/display.h"
include "../lib/radprof.h"

# AP_RPARS -- Procedure to write the radprof parameters to the parameter
# file.

procedure ap_rpars (ap)

pointer	ap		# pointer to apphot structure

bool	itob()
int	apstati()
real	apstatr()

begin
	# Write the data dependent parameters.
	call ap_dapars (ap)

	# Write the centering parameters.
	call ap_cepars (ap)

	# Write the sky fitting parameters.
	call ap_sapars (ap)

	# Write out the photometry parameters.
	call ap_phpars (ap)

	# Set the radphot parameters
	call clputr ("radius", apstatr (ap, RPRADIUS))
	call clputr ("step", apstatr (ap, RPSTEP))
	call clputi ("order", apstati (ap, RPORDER))
	call clputr ("kreject", apstatr (ap, RPKSIGMA))
	call clputi ("nreject", apstati (ap, RPNREJECT))

	# Set radial profile plots
	call clputb ("radplots", itob (apstati (ap, RADPLOTS)))
end
