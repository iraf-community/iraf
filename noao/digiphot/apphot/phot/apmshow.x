include "../lib/display.h"

# AP_MSHOW -- Procedure to print the photometry parameters on the standard
# output.

procedure ap_mshow (ap)

pointer	ap		# pointer to apphot structure

bool	itob()
int	apstati()
begin
	call ap_nshow (ap)
	call printf ("\n")
	call ap_mpshow (ap)
	call printf ("    %s = %b\n")
	    call pargstr (KY_RADPLOTS)
	    call pargb (itob (apstati (ap, RADPLOTS)))
end
