include "../lib/apphot.h"
include "../lib/noise.h"

# AP_FDCONFIRM -- Procedure to confirm the critical daofind parameters.

procedure ap_fdconfirm (ap)

pointer	ap		# pointer to the apphot structure

real	scale, fwhmpsf, threshold
int	scan(), nscan()
real	apstatr()

begin
	call printf ("\n")

	scale = apstatr (ap, SCALE) 

	# Confirm the fwhmpsf.
	call printf ("FWHM of features in scale units (%g): ")
	    call pargr (apstatr (ap, FWHMPSF))
	call flush (STDOUT)
	if (scan() == EOF)
	    fwhmpsf = apstatr (ap, FWHMPSF)
	else {
	    call gargr (fwhmpsf)
	    if (nscan () != 1)
	        fwhmpsf = apstatr (ap, FWHMPSF)
	}
	call printf ("\tNew FWHM of features: %g scale units  %g pixels\n")
	    call pargr (fwhmpsf)
	    call pargr (scale * fwhmpsf)

	# Confirm the threshold parameter.
	call printf ("Detection threshold in counts above background (%g): ")
	    call pargr (apstatr (ap, THRESHOLD))
	call flush (STDOUT)
	if (scan() == EOF)
	    threshold = apstatr (ap, THRESHOLD)
	else {
	    call gargr (threshold)
	    if (nscan () != 1)
	        threshold = apstatr (ap, THRESHOLD)
	}
	call printf ("\tNew detection threshold: %g counts\n")
	    call pargr (threshold)

	call printf ("\n")

	call apsetr (ap, FWHMPSF, fwhmpsf)
	call apsetr (ap, THRESHOLD, threshold)
end
