include "../lib/apphot.h"
include "../lib/noise.h"
include "../lib/center.h"
include "../lib/fitsky.h"
include "../lib/phot.h"

# AP_WCONFIRM -- Procedure to confirm the critical phot parameters.

procedure ap_wconfirm (ap)

pointer	ap		# pointer to the apphot structure

int	i, cfunc, sfunc, wfunc, naperts
pointer	sp, str, aperts
real	scale, fwhmpsf, capert, cthreshold, annulus, dannulus, skysigma
int	apstati(), scan(), nscan(), strdic(), ap_getaperts()
real	apstatr()

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (aperts, MAX_NAPERTS, TY_REAL)

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
	call printf ( "\tNew FWHM of features: %g in scale units %g pixels\n")
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
	    call printf ("Centering box width in scale unit (%g): ")
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

	# Confirm the weighting scheme algorithm.
	call apstats (ap, PWSTRING, Memc[str], SZ_LINE)
	call printf ("Weighting algorithm (%s): ")
	    call pargstr (Memc[str])
	call flush (STDOUT)
	if (scan() != EOF) {
	    call gargwrd (Memc[str], SZ_LINE)
	    wfunc = strdic (Memc[str], Memc[str], SZ_LINE, PWFUNCS)
	    if (nscan () == 1 && wfunc > 0) {
		call apseti (ap, PWEIGHTS, wfunc)
		call apsets (ap, PWSTRING, Memc[str])
	    }
	}
	call apstats (ap, PWSTRING, Memc[str], SZ_LINE)
	call printf ("\tNew weighting algorithm: %s\n")
	    call pargstr (Memc[str])

	# Confirm the aperture radii parameter.
	call apstats (ap, APERTS, Memc[str], SZ_LINE)
	call printf ("File/list of aperture radii in scale units (%s): ")
	    call pargstr (Memc[str])
	call flush (STDOUT)
	if (scan() == EOF)
	    call apstats (ap, APERTS, Memc[str], SZ_LINE)
	else {
	    call gargwrd (Memc[str], SZ_LINE)
	    if (nscan () != 1)
	        call apstats (ap, APERTS, Memc[str], SZ_LINE)
	}

	naperts = ap_getaperts (Memc[str], Memr[aperts], MAX_NAPERTS)
	do i = 1, naperts {
	    call printf ("\tAperture radius %d: %g scale units %g pixels\n")
		call pargi (i)
		call pargr (Memr[aperts+i-1])
		call pargr (scale * Memr[aperts+i-1])
	}

	call printf ("\n")

	call apsetr (ap, FWHMPSF, fwhmpsf)
	call apsetr (ap, CAPERT, capert / 2.0)
	call apsetr (ap, CTHRESHOLD, cthreshold)
	call apsetr (ap, ANNULUS, annulus)
	call apsetr (ap, DANNULUS, dannulus)
	call apsetr (ap, SKYSIGMA, skysigma)
	call apsets (ap, APERTS, Memc[str])
end
