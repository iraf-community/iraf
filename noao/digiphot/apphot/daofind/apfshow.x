include "../lib/apphot.h"
include "../lib/noise.h"
include "../lib/find.h"

# AP_FSHOW -- Procedure to display the current data parameters.

procedure ap_fshow (ap)

pointer	ap	# pointer to the apphot strucuture

pointer	sp, str
int	apstati(), itob()
real	apstatr()

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Set the object charactersitics.
	call printf ("Kernel Parameters\n")
	call printf ("    %s = %g %s    %s = %b\n")
	    call pargstr (KY_FWHMPSF)
	    call pargr (apstatr (ap, FWHMPSF))
	    call pargstr (UN_FWHMPSF)
	    call pargstr (KY_POSITIVE)
	    call pargb (itob (apstati (ap, POSITIVE)))

	call printf ("    %s = %g %s    %s = %g   %s = %g %s\n")
	    call pargstr (KY_NSIGMA)
	    call pargr (apstatr (ap, NSIGMA))
	    call pargstr (UN_NSIGMA)
	    call pargstr (KY_RATIO)
	    call pargr (apstatr (ap, RATIO))
	    call pargstr (KY_THETA)
	    call pargr (apstatr (ap, THETA))
	    call pargstr (UN_THETA)

	# Print the rest of the data dependent parameters.
	call printf ("\nDetection Parameters\n")
	call printf ("    %s = %g %s\n")
	    call pargstr (KY_THRESHOLD)
	    call pargr (apstatr (ap, THRESHOLD))
	    call pargstr (UN_THRESHOLD)

	call printf ("    %s = %g    %s = %g\n")
	    call pargstr (KY_SHARPLO)
	    call pargr (apstatr (ap, SHARPLO))
	    call pargstr (KY_SHARPHI)
	    call pargr (apstatr (ap, SHARPHI))

	call printf ("    %s = %g    %s = %g\n")
	    call pargstr (KY_ROUNDLO)
	    call pargr (apstatr (ap, ROUNDLO))
	    call pargstr (KY_ROUNDHI)
	    call pargr (apstatr (ap, ROUNDHI))

	call sfree (sp)
end
