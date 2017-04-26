include "../lib/display.h"
include "../lib/noise.h"
include "../lib/center.h"

# AP_GCPARS -- Procedure to read in the centering parameters from the
# appropriate parameters files.

procedure ap_gcpars (ap)

pointer	ap			# pointer to the apphot structure

bool	clgetb()
int	btoi()

begin
	# Initialize the structure.
	call apcinit (ap, AP_CENTROID1D, 2.5, 2.0, AP_NPOISSON)

	# Get the data dependent parameters.
	call ap_gdapars (ap)

	# Get the centering algorithm parameters.
	call ap_gcepars (ap)

	# Make radial plots on stdgraph.
	call apseti (ap, RADPLOTS, btoi (clgetb ("radplots")))
end
