include "../lib/apphot.h"
include "../lib/noise.h"
include "../lib/center.h"
include "../lib/fitsky.h"

# AP_YCONFIRM -- Procedure to confirm the critical polyphot parameters.

procedure ap_yconfirm (ap)

pointer	ap		# pointer to the apphot structure

int	cfunc, sfunc
pointer	sp, str
real	scale, fwhmpsf, capert, cthreshold, annulus, dannulus, skysigma
int	apstati(), scan(), nscan(), strdic()
real	apstatr()

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	call printf ("\n")

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
	call printf ( "\tNew FWHM of features: %g scale units %g pixels\n")
	    call pargr (fwhmpsf)
	    call pargr (scale * fwhmpsf)

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
	        "\tNew centering box width: %g scale units %g pixels\n")
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

	# Confirm the sky fitting algorithm.
	call apstats (ap, SSTRING, Memc[str], SZ_LINE)
	call printf ("Sky fitting algorithm (%s): ")
	    call pargstr (Memc[str])
	call flush (STDOUT)
	if (scan() != EOF) {
	    call gargwrd (Memc[str], SZ_LINE)
	    sfunc = strdic (Memc[str], Memc[str], SZ_LINE, SFUNCS)
	    if (nscan () == 1 && sfunc > 0) {
		call apseti (ap, SKYFUNCTION, sfunc)
		call apsets (ap, SSTRING, Memc[str])
	    }
	}
	call apstats (ap, SSTRING, Memc[str], SZ_LINE)
	call printf ("\tSky fitting algorithm: %s\n")
	    call pargstr (Memc[str])

	if (apstati (ap, SKYFUNCTION) != AP_CONSTANT &&
	    apstati (ap, SKYFUNCTION) != AP_SKYFILE) {

	    # Confirm the sky annulus parameter.
	    call printf ("Inner radius of sky annulus in scale units (%g): ")
	        call pargr (apstatr (ap, ANNULUS))
	    call flush (STDOUT)
	    if (scan() == EOF)
	        annulus = apstatr (ap, ANNULUS)
	    else {
	        call gargr (annulus)
	        if (nscan () != 1)
	            annulus = apstatr (ap, ANNULUS)
	    }
	    call printf (
	        "\tNew inner radius of sky annulus: %g scale units %g pixels\n")
	        call pargr (annulus)
	        call pargr (scale * annulus)

	    # Confirm the width of the sky annulus.
	    call printf ("Width of the sky annulus in scale (%g): ")
	        call pargr (apstatr (ap, DANNULUS))
	    call flush (STDOUT)
	    if (scan() == EOF)
	        dannulus = apstatr (ap, DANNULUS)
	    else {
	        call gargr (dannulus)
	        if (nscan () != 1)
	            dannulus = apstatr (ap, DANNULUS)
	    }
	    call printf (
	        "\tNew width of the sky annulus: %g scale %g pixels\n")
	        call pargr (dannulus)
	        call pargr (scale * dannulus)

	} else {
	    annulus = apstatr (ap, ANNULUS)
	    dannulus = apstatr (ap, DANNULUS)
	}

	# Confirm the sky sigma parameter.
	if (apstati (ap, SKYFUNCTION) != AP_SKYFILE) {
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
	} else
	    skysigma = apstatr (ap, SKYSIGMA)

	call printf ("\n")

	call apsetr (ap, FWHMPSF, scale)
	call apsetr (ap, CAPERT, capert / 2.0)
	call apsetr (ap, CTHRESHOLD, cthreshold)
	call apsetr (ap, ANNULUS, annulus)
	call apsetr (ap, DANNULUS, dannulus)
	call apsetr (ap, SKYSIGMA, skysigma)
end
