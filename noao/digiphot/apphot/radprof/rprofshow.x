include "../lib/display.h"

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
