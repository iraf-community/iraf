include "../lib/display.h"
include "../lib/noise.h"
include "../lib/center.h"
include "../lib/fitsky.h"
include "../lib/phot.h"

# AP_GPPARS -- Procedure to fetch the phot task parameters.

procedure ap_gppars (ap)

pointer	ap		# pointer to apphot structure

bool	clgetb()
int	btoi()

begin
	# Open the apphot strucuture.
	call appinit (ap, AP_CENTROID1D, 2.5, AP_MODE, 10.0, 10.0, 3.0, 1,
	    AP_PWCONSTANT, 2.0, AP_NPOISSON)

	# Get the data dependent parameters.
	call ap_gdapars (ap)

	# Get the centering algorithm parameters.
	call ap_gcepars (ap)

	# Get the sky fitting parameters.
	call ap_gsapars (ap)

	# Get the photometry parameters.
	call ap_gphpars (ap)

	# Get the plotting parameters.
	call apseti (ap, RADPLOTS, btoi (clgetb ("radplots")))
end
