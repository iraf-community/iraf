include "../lib/display.h"

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
