include "../lib/apphot.h"
include "../lib/noise.h"
include "../lib/fitsky.h"

# AP_SCONFIRM -- Procedure to confirm the critical fitsky parameters.

procedure ap_sconfirm (ap)

pointer	ap		# pointer to the apphot structure

int	sfunc
pointer	sp, str
real	scale, annulus, dannulus, skysigma
int	apstati(), scan(), nscan(), strdic()
real	apstatr()

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	call printf ("\n")

	scale = apstatr (ap, SCALE)

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
	    call printf ("Width of the sky annulus in scale units (%g): ")
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
	        "\tNew width of the sky annulus: %g scale units %g pixels\n")
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

	call apsetr (ap, ANNULUS, annulus)
	call apsetr (ap, DANNULUS, dannulus)
	call apsetr (ap, SKYSIGMA, skysigma)
end
