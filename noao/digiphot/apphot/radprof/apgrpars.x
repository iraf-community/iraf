include "../lib/display.h"
include "../lib/noise.h"
include "../lib/center.h"
include "../lib/fitsky.h"
include "../lib/phot.h"
include "../lib/radprof.h"

# AP_GRPPARS -- Procedure to fetch the radprof parameters.

procedure ap_grppars (ap)

pointer	ap		# pointer to apphot structure

bool	clgetb()
int	clgeti(), btoi()
real	clgetr()

begin
	# Open the apphot strucuture.
	call ap_rpinit (ap, AP_CENTROID1D, 2.5, AP_MODE, 10.0, 10.0,
	        3.0, 1, 8.0, 0.5, 2.0, AP_NPOISSON) 

	# Get the radial profile parameters.
	call apsetr (ap, RPRADIUS, clgetr ("radius"))
	call apsetr (ap, RPSTEP, clgetr ("step"))

	# Get the data dependent parameters.
	call ap_gdapars (ap)

	# Get the centering algorithm parameters.
	call ap_gcepars (ap)

	# Get the sky fitting algorithm parameters.
	call ap_gsapars (ap)

	# Get the photometry parameters.
	call ap_gphpars (ap)

	# Set remainder of the radprof parameters.
	call apsetr (ap, RPKSIGMA, clgetr ("kreject"))
	call apseti (ap, RPNREJECT, clgeti ("nreject"))
	call apseti (ap, RPORDER, clgeti ("order"))

	# Set the plotting parameters.
	call apseti (ap, RADPLOTS, btoi (clgetb ("radplots")))
end
