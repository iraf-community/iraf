include "../lib/center.h"
include "../lib/display.h"

# AP_CPSHOW -- Procedure to display the current centering algorithm
# parameters.

procedure ap_cpshow (ap)

pointer	ap	# pointer to the apphot strucuture

pointer	sp, str
bool	itob()
int	apstati()
real	apstatr()

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Print the centering algorithm.
	call printf ("Centering Parameters\n")
	call apstats (ap, CSTRING, Memc[str], SZ_FNAME)
	call printf ("    %s = %s %s\n")
	    call pargstr (KY_CSTRING)
	    call pargstr (Memc[str])
	    call pargstr (UN_CALGORITHM)

	# Print the rest of the centering parameters.
	call printf ("    %s = %g %s    %s = %g %s    %s = %g\n")
	    call pargstr (KY_CAPERT)
	    call pargr (2.0 * apstatr (ap, CAPERT))
	    call pargstr (UN_CSCALEUNIT)
	    call pargstr (KY_CTHRESHOLD)
	    call pargr (apstatr (ap, CTHRESHOLD))
	    call pargstr (UN_CSIGMA)
	    call pargstr (KY_MINSNRATIO)
	    call pargr (apstatr (ap, MINSNRATIO))

	call printf ("    %s = %d    %s = %g %s\n")
	    call pargstr (KY_CMAXITER)
	    call pargi (apstati (ap, CMAXITER))
	    call pargstr (KY_MAXSHIFT)
	    call pargr (apstatr (ap, MAXSHIFT))
	    call pargstr (UN_CNUMBER)

	call printf ("    %s = %b    %s = %g %s\n")
	    call pargstr (KY_CLEAN)
	    call pargb (itob (apstati (ap, CLEAN)))
	    call pargstr (KY_SIGMACLEAN)
	    call pargr (apstatr (ap, SIGMACLEAN))
	    call pargstr (UN_CSIGMA)

	call printf ("    %s = %g %s    %s = %g %s\n")
	    call pargstr (KY_RCLEAN)
	    call pargr (apstatr (ap, RCLEAN))
	    call pargstr (UN_CSCALEUNIT)
	    call pargstr (KY_RCLIP)
	    call pargr (apstatr (ap, RCLIP))
	    call pargstr (UN_CSCALEUNIT)

	call printf ("    %s = %b\n")
	    call pargstr (KY_MKCENTER)
	    call pargb (itob (apstati (ap, MKCENTER)))

	call sfree (sp)
end
