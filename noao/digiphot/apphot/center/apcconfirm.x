include "../lib/apphot.h"
include "../lib/noise.h"
include "../lib/center.h"

# AP_CCONFIRM -- Procedure to confirm the critical center parameters.

procedure ap_cconfirm (ap)

pointer	ap		# pointer to the apphot structure

int	cfunc
pointer	sp, str
real	scale, fwhmpsf, capert, cthreshold, skysigma
int	apstati(), scan(), nscan(), strdic()
real	apstatr()

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call printf ("\n")

	# Get the apphot scale factor.
	scale = apstatr (ap, SCALE)

	# Confirm the fwhmpsf.
	call printf ( "FWHM of features in scale units (%g): ")
	    call pargr (apstatr (ap, FWHMPSF))
	call flush (STDOUT)
	if (scan() == EOF)
	    fwhmpsf = apstatr (ap, FWHMPSF)
	else {
	    call gargr (fwhmpsf)
	    if (nscan () != 1)
	        fwhmpsf = apstatr (ap, FWHMPSF)
	}
	call printf ( "\tNew FWHM of features: %g scale units  %g pixels\n")
	    call pargr (fwhmpsf)
	    call pargr (fwhmpsf * scale)

	# Confirm the centering algorithm.
	call apstats (ap, CSTRING, Memc[str], SZ_LINE)
	call printf ("Centering algorithm (%s): ")
	    call pargstr (Memc[str])
	call flush (STDOUT)
	if (scan() != EOF) {
	    call gargwrd (Memc[str], SZ_LINE)
	    cfunc = strdic (Memc[str], Memc[str], SZ_LINE, CFUNCS)
	    if (nscan () == 1 && cfunc > 0) {
		call apseti (ap, CENTERFUNCTION, cfunc)
		call apsets (ap, CSTRING, Memc[str])
	    }
	}
	call apstats (ap, CSTRING, Memc[str], SZ_LINE)
	call printf ("\tNew centering algorithm: %s\n")
	    call pargstr (Memc[str])

	if (apstati (ap, CENTERFUNCTION) != AP_NONE) {

	    # Confirm the centering box.
	    call printf ("Centering box width in scale units (%g): ")
	        call pargr (2.0 * apstatr (ap, CAPERT))
	    call flush (STDOUT)
	    if (scan() == EOF)
	        capert = 2.0 * apstatr (ap, CAPERT)
	    else {
	        call gargr (capert)
	        if (nscan () != 1)
	            capert = 2.0 * apstatr (ap, CAPERT)
	    }
	    call printf (
	        "\tNew centering box width: %g scale units  %g pixels\n")
	        call pargr (capert)
	        call pargr (scale * capert)

	    # Confirm the centering threshold parameter.
	    call printf (
	        "Centering threshold in counts above background (%g): ")
	        call pargr (apstatr (ap, CTHRESHOLD))
	    call flush (STDOUT)
	    if (scan() == EOF)
	        cthreshold = apstatr (ap, CTHRESHOLD)
	    else {
	        call gargr (cthreshold)
	        if (nscan () != 1)
	            cthreshold = apstatr (ap, CTHRESHOLD)
	    }
	    call printf ("\tNew centering threshold: %g counts\n")
	        call pargr (cthreshold)

	} else {
	    capert = 2.0 * apstatr (ap, CAPERT)
	    cthreshold = apstatr (ap, CTHRESHOLD)
	}

	# Confirm the sky sigma parameter.
	call printf ("Standard deviation of background in counts (%g): ")
	    call pargr (apstatr (ap, SKYSIGMA))
	call flush (STDOUT)
	if (scan() == EOF)
	    skysigma = apstatr (ap, SKYSIGMA)
	else {
	    call gargr (skysigma)
	    if (nscan () != 1)
	        skysigma = apstatr (ap, SKYSIGMA)
	}
	call printf ("\tNew standard deviation of background: %g counts\n")
	    call pargr (skysigma)

	call printf ("\n")

	# Store the confirmed parameters.
	call apsetr (ap, FWHMPSF, fwhmpsf)
	call apsetr (ap, CAPERT, capert / 2.0)
	call apsetr (ap, CTHRESHOLD, cthreshold)
	call apsetr (ap, SKYSIGMA, skysigma)
end
