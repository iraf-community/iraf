include "../lib/apphot.h"
include "../lib/noise.h"
include "../lib/fitpsf.h"

# AP_PFCONFIRM -- Procedure to confirm the critical fitpsf parameters.

procedure ap_pfconfirm (ap)

pointer	ap		# pointer to the apphot structure

int	pfunc
pointer	sp, str
real	scale, fwhmpsf, psfapert, threshold, skysigma
int	apstati(), scan(), nscan(), strdic()
real	apstatr()

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	call printf ("\n")

	scale = apstatr (ap, SCALE)

	# Confirm the fwhmpsf.
	call printf ( "FWHM of features in pixels (%g): ")
	    call pargr (apstatr (ap, FWHMPSF))
	call flush (STDOUT)
	if (scan() == EOF)
	    fwhmpsf = apstatr (ap, FWHMPSF)
	else {
	    call gargr (fwhmpsf)
	    if (nscan () != 1)
	        fwhmpsf = apstatr (ap, FWHMPSF)
	}
	call printf ( "\tNew FWHM of features: %g in scale units  %g pixels\n")
	    call pargr (fwhmpsf)
	    call pargr (scale * fwhmpsf)

	# Confirm the fitting function.
	call apstats (ap, PSFSTRING, Memc[str], SZ_LINE)
	call printf ("Fitting function (%s): ")
	    call pargstr (Memc[str])
	call flush (STDOUT)
	if (scan() != EOF) {
	    call gargwrd (Memc[str], SZ_LINE)
	    pfunc = strdic (Memc[str], Memc[str], SZ_LINE, PSFFUNCS)
	    if (nscan () == 1 && pfunc > 0) {
		call apseti (ap, PSFUNCTION, pfunc)
		call apsets (ap, PSFSTRING, Memc[str])
	    }
	}
	call apstats (ap, PSFSTRING, Memc[str], SZ_LINE)
	call printf ("\tNew fitting function: %s\n")
	    call pargstr (Memc[str])

	# Confirm the fitting box.
	call printf ("Fitting box width in scale units (%g): ")
	    call pargr (2.0 * apstatr (ap, PSFAPERT))
	call flush (STDOUT)
	if (scan() == EOF)
	    psfapert = 2.0 * apstatr (ap, PSFAPERT)
	else {
	    call gargr (psfapert)
	    if (nscan () != 1)
	        psfapert = 2.0 * apstatr (ap, PSFAPERT)
	}
	call printf ("\tNew fitting box width: %g scale units  %g pixels\n")
	    call pargr (psfapert)
	    call pargr (scale 	* psfapert)

	# Confirm the centering threshold parameter.
	if (apstati (ap, PSFUNCTION) == AP_MOMENTS) {
	    call printf (
	        "Threshold in counts above background (%g): ")
	        call pargr (apstatr (ap, THRESHOLD))
	    call flush (STDOUT)
	    if (scan() == EOF)
	        threshold = apstatr (ap, THRESHOLD)
	    else {
	        call gargr (threshold)
	        if (nscan () != 1)
	            threshold = apstatr (ap, THRESHOLD)
	    }
	    call printf ("\tNew threshold: %g counts\n")
	        call pargr (threshold)
	} else
	    threshold = apstatr (ap, THRESHOLD)

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

	call apsetr (ap, FWHMPSF, fwhmpsf)
	call apsetr (ap, PSFAPERT, psfapert / 2.0)
	call apsetr (ap, THRESHOLD, threshold)
	call apsetr (ap, SKYSIGMA, skysigma)
end
