include "../lib/display.h"
include "../lib/radprof.h"

# AP_RPROFSHOW -- Procedure to display all the radprof parameters.

procedure ap_rprofshow (ap)

pointer	ap	# pointer to the apphot strucuture

bool	itob()
int	apstati()

begin
	call ap_nshow (ap)
	call printf ("\n")
	call ap_cpshow (ap)
	call printf ("\n")
	call ap_spshow (ap)
	call printf ("\n")
	call ap_mpshow (ap)
	call printf ("\n")
	call ap_rppshow (ap)
	call printf ("    %s = %b\n")
	    call pargstr (KY_RADPLOTS)
	    call pargb (itob (apstati (ap, RADPLOTS)))
end


# AP_RPSHOW -- Procedure to display radprof parameters.

procedure ap_rpshow (ap)

pointer	ap		# pointer to apphot structure

bool	itob()
int	apstati()

begin
	call ap_nshow (ap)
	call ap_rppshow (ap)
	call printf ("    %s = %b\n")
	    call pargstr (KY_RADPLOTS)
	    call pargb (itob (apstati (ap, RADPLOTS)))
end


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
	    call pargstr (UN_RSCALEUNIT)
	    call pargstr (KY_RPSTEP)
	    call pargr (apstatr (ap, RPSTEP))
	    call pargstr (UN_RSCALEUNIT)

	call printf ("    %s = %d    %s = %g %s    %s = %d\n")
	    call pargstr (KY_RPORDER)
	    call pargi (apstati (ap, RPORDER))
	    call pargstr (KY_RPKSIGMA)
	    call pargr (apstatr (ap, RPKSIGMA))
	    call pargstr (UN_RSIGMA)
	    call pargstr (KY_RPNREJECT)
	    call pargi (apstati (ap, RPNREJECT))
end
