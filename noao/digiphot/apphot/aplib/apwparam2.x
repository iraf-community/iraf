include "../lib/apphotdef.h"
include "../lib/polyphot.h"
include "../lib/fitpsf.h"
include "../lib/radprof.h"


# AP_WPROFS -- Procedure to print out the profile fitting parameters.

procedure ap_wprofs (ap, out)

pointer	ap		# apphot structure pointer
pointer	out		# output file descriptor

pointer sp, str
int	apstati()
real	apstatr()

begin
	if (out == NULL)
	    return

	call smark (sp)
	call salloc (str, SZ_FNAME, TY_CHAR)

	if (AP_RPROF(ap) != NULL) {
	    call ap_rparam (out, KY_RPRADIUS, apstatr (ap, RPRADIUS),
	        UN_RPRADIUS, "fitting radius")
	    call ap_rparam (out, KY_RPSTEP, apstatr (ap, RPSTEP),
	        UN_RPSTEP, "step size in radius")
	    call ap_iparam (out, KY_RPORDER, apstati (ap, RPORDER), UN_RPORDER,
		"number of splines pieces")
	    call ap_rparam (out, KY_RPKSIGMA, apstatr (ap, RPKSIGMA),
	        UN_RPKSIGMA, "k-sigma rejection criterion")
	    call ap_iparam (out, KY_RPNREJECT, apstati (ap, RPNREJECT),
	        UN_RPNREJECT, "maximum number of rejection cycles")
	    call fprintf (out, "#\n")
	}

	call sfree (sp)
end


# AP_WPOLY -- Procedure to write out the polyphot parameters.

procedure ap_wpoly (ap, out)

pointer	ap		# apphot structure pointer
pointer	out		# output file descriptor

pointer	sp, str
real	apstatr()

begin
	if (out == NULL)
	    return

	call smark (sp)
	call salloc (str, SZ_FNAME, TY_CHAR)

	if (AP_POLY(ap) != NULL) {
	    call ap_sparam (out, "WEIGHTING", "constant", "model", "")
	    call ap_rparam (out, KY_PYZMAG, apstatr (ap, PYZMAG), UN_PYZMAG,
		"zero point of magnitdue scale")
	    call fprintf (out, "#\n")
	}

	call sfree (sp)
end


# AP_WPSF -- Procedure to write the psf fitting parameters .

procedure ap_wpsf (ap, out)

pointer	ap		# apphot strucuture pointer
pointer	out		# output file descriptor

pointer	sp, str
int	apstati()
real	apstatr()

begin
	if (out == NULL)
	    return
	call smark (sp)
	call salloc (str, SZ_FNAME, TY_CHAR)

	if (AP_PPSF(ap) != NULL) {
	    call apstats (ap, PSFSTRING, Memc[str], SZ_FNAME)
	    call ap_sparam (out, KY_PSFUNCTION, Memc[str], UN_PSFUNCTION,
		"fitting function")
	    call ap_rparam (out, KY_PSFAPERT, 2.0 * apstatr (ap, PSFAPERT),
	        UN_PSFAPERT, "width of the fitting box")
	    call ap_iparam (out, KY_PMAXITER, apstati (ap, PMAXITER),
	        UN_PMAXITER, "maximum number of iterations")
	    call ap_rparam (out, KY_PK2, apstatr (ap, PK2), UN_PK2,
		"k-sigma rejection limit for the fit")
	    call ap_iparam (out, KY_PNREJECT, apstati (ap, PNREJECT),
	        UN_PNREJECT, "maximum number of rejection cycles")
	    call fprintf (out, "#\n")
	}

	call sfree (sp)
end
