include "../lib/noise.h"
include "../lib/display.h"
include "../lib/find.h"

# AP_FDGPARS -- Open up the apphot data structure and get the daofind input
# parameters.

procedure ap_fdgpars (ap)

pointer	ap			# pointer to the apphot structure

bool	clgetb()
int	btoi()
real	clgetr()

begin
	# Open the apphot structure.
	call ap_fdinit (ap, 2.0, AP_NPOISSON)

	# Get the data dependent parameters.
	call ap_gdapars (ap)

	# Get the remaining kernel statistics.
	call apsetr (ap, RATIO, clgetr ("ratio"))
	call apsetr (ap, THETA, clgetr ("theta"))
	call apsetr (ap, NSIGMA, clgetr ("nsigma"))

	# Get the image detection characteristics.
	call apsetr (ap, SHARPLO, clgetr ("sharplo"))
	call apsetr (ap, SHARPHI, clgetr ("sharphi"))
	call apsetr (ap, ROUNDLO, clgetr ("roundlo"))
	call apsetr (ap, ROUNDHI, clgetr ("roundhi"))

	call apseti (ap, MKDETECTIONS, btoi (clgetb ("mkdetections")))
end
