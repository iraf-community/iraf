include "../lib/radprof.h"

# AP_RPPSHOW -- Procedure to display radprof parameters.

procedure ap_rppshow (ap)

pointer	ap		# pointer to apphot structure

int	apstati()
real	apstatr()

begin
	# Print the radial profile characteristics.
	call printf ("Radial Profile Fitting Parameters\n")
	call printf ("    %s = %g %s    %s = %g %s\n")
	    call pargstr (KY_RPRADIUS)
	    call pargr (apstatr (ap, RPRADIUS))
	    call pargstr (UN_RPRADIUS)
	    call pargstr (KY_RPSTEP)
	    call pargr (apstatr (ap, RPSTEP))
	    call pargstr (UN_RPSTEP)

	call printf ("    %s = %d    %s = %g %s    %s = %d\n")
	    call pargstr (KY_RPORDER)
	    call pargi (apstati (ap, RPORDER))
	    call pargstr (KY_RPKSIGMA)
	    call pargr (apstatr (ap, RPKSIGMA))
	    call pargstr (UN_RPKSIGMA)
	    call pargstr (KY_RPNREJECT)
	    call pargi (apstati (ap, RPNREJECT))
end
