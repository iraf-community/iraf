include "../lib/display.h"
include "../lib/find.h"

# AP_FDPARS -- Write out the current daofind parameters to the current
# parameter files.

procedure ap_fdpars (ap)

pointer	ap		# pointer to apphot structure

bool	itob()
int	apstati()
real	apstatr()

begin
	# Write the data dependent parameters.
	call ap_dapars (ap)

	# Write the daofind parameters.
	call clputr ("nsigma", apstatr (ap, NSIGMA) )
	call clputr ("ratio", apstatr (ap, RATIO))
	call clputr ("theta", apstatr (ap, THETA))
	call clputr ("sharplo", apstatr (ap, SHARPLO))
	call clputr ("sharphi", apstatr (ap, SHARPHI))
	call clputr ("roundlo", apstatr (ap, ROUNDLO))
	call clputr ("roundhi", apstatr (ap, ROUNDHI))

	call clputb ("mkdetections", itob (apstati (ap, MKDETECTIONS)))
end
