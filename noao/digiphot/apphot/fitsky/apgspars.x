include "../lib/display.h"
include "../lib/noise.h"
include "../lib/fitsky.h"

# AP_SGPARS -- Procedure to fetch the parameters for the fitsky task.

procedure ap_sgpars (ap)

pointer	ap		# pointer to apphot structure

bool	clgetb()
int	btoi()

begin
	# Open the apphot structure.
	call apsinit (ap, AP_MODE, 10.0, 10.0, 2.0, AP_NPOISSON)

	# Get the data dependent parameters.
	call ap_gdapars (ap)

	# Get the sky fitting parameters.
	call ap_gsapars (ap)

	# Get radial plots.
	call apseti (ap, RADPLOTS, btoi (clgetb ("radplots")))
end
