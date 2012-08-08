include "../lib/display.h"
include "../lib/noise.h"
include "../lib/center.h"
include "../lib/fitsky.h"
include "../lib/polyphot.h"

# AP_GYPARS -- Procedure to fetch the polyphot task parameters.

procedure ap_gypars (ap)

pointer	ap		# pointer to apphot fitting structure

begin
	# Open the apphot strucuture.
	call apyinit (ap, AP_CENTROID1D, 2.5, AP_MODE, 10.0, 10.0, 2.0,
	    AP_NPOISSON)

	# Get the data dependent parameters.
	call ap_gdapars (ap)

	# Get the centering algorithm parameters.
	call ap_gcepars (ap)

	# Get the sky fitting parameters.
	call ap_gsapars (ap)

	# Get the photometry parameters.
	call ap_gpopars (ap)
end
